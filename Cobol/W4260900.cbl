000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4260900.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           MAJ 1990.                                        
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        INFIL    FRÅN W42605                                             
001100*                                                                         
001200*        LISTA    KVALITET FELFÖRDELNING TILL CDC                         
001300*                                                                         
001310*        LISTFIL  KVALITET FELFÖRDELNING TILL SDC:ER                      
001320*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SELECT W42601       ASSIGN TO W42609D1.                              
002010*                        * INFIL                                          
002100     SELECT W42609-001   ASSIGN TO W42609D2.                              
002110*                        * LISTA   TILL CDC   PÅ SVENSKA                  
002120                                                                          
002200     SELECT W42609-001A  ASSIGN TO W42609D3.                              
002210*                        * LISTA   TILL CDC   PÅ ENGELSKA                 
002220                                                                          
002300     SELECT W42609DC21   ASSIGN TO W42609D4.                              
002310*                        * LISTFIL TILL DC21  PÅ FLAMLÄNDSKA              
002320                                                                          
002400     SELECT W42610DC21   ASSIGN TO W42609D5.                              
002410*                        * LISTFIL TILL DC21  PÅ ENGELSKA                 
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W42601                                                               
003100     LABEL RECORD STANDARD                                                
003200     RECORDING F                                                          
003300     BLOCK CONTAINS 0.                                                    
003400*01  W42601-POST -COPY W4260501     -L                                    
003600     SKIP2                                                                
003610                                                                          
003700 FD  W42609-001                                                           
003800     LABEL RECORD STANDARD.                                               
003900 01  W42609-001-LISTA-S-CDC.                                              
004000     03  RAD-SKIP-S-CDC          PIC S9(03) COMP-3.                       
004100     03  FILLER                  PIC X(119).                              
004200     SKIP2                                                                
004210                                                                          
004300 FD  W42609-001A                                                          
004400     LABEL RECORD STANDARD.                                               
004500 01  W42609-001A-LISTA-GB-CDC.                                            
004600     03  RAD-SKIP-GB-CDC         PIC S9(03) COMP-3.                       
004700     03  FILLER                  PIC X(119).                              
004800     EJECT                                                                
004810                                                                          
004900 FD  W42609DC21                                                           
005000     LABEL RECORD STANDARD.                                               
005100 01  W42609-LISTFIL-NL-DC21.                                              
005200     03  RAD-SKIP-NL-DC21        PIC S9(03) COMP-3.                       
005300     03  FILLER                  PIC X(119).                              
005400     SKIP2                                                                
005410                                                                          
005500 FD  W42610DC21                                                           
005600     LABEL RECORD STANDARD.                                               
005700 01  W42610-LISTFIL-GB-DC21.                                              
005800     03  RAD-SKIP-GB-DC21        PIC S9(03) COMP-3.                       
005900     03  FILLER                  PIC X(119).                              
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200     SKIP2                                                                
006201                                                                          
006210*    -- CHECKED BY WY2000                                                 
006300*    ---- ARBETSVARIABLER                                                 
006400*                                                                         
006500 77  IDPGM                   PIC  X(08)  VALUE 'W4260900'.                
006600 77  INFIL-SLUT              PIC  X(03)  VALUE SPACE.                     
006700 77  W-SUM-KVKVAFPO          PIC  9(07).                                  
006800 77  W-PROCENT               PIC  9(03).                                  
006900 77  W-ANT-RADER             PIC S9(03)  COMP-3 VALUE +0.                 
007000 77  W-BRYT                  PIC  X(01)  VALUE 'J'.                       
007100 01  IN-W42601.                                                           
007200     03  IN-IDPTYP           PIC X(03).                                   
007300     03  FILLER              PIC X(200).                                  
007400*                                                                         
007500 77  JA                      PIC X(01)   VALUE 'J'.                       
007600 77  NEJ                     PIC X(01)   VALUE 'N'.                       
007700 77  C1                      PIC S9(01)  COMP-3 VALUE +1.                 
007800                                                                          
007810*      --- VALID IDDC CODES                                               
007820*                                                                         
007830*01    -COPY WWDC99                                                       
007840       EJECT                                                              
007900*    ---- INDEXFÄLT                                                       
008000 77  IX1                     PIC S9(3)   VALUE +0   COMP SYNC.            
008100     EJECT                                                                
008200 01  RUBRIK1.                                                             
008300   03  RUBRIK1-S.                                                         
008400      05  FILLER             PIC S9(3) COMP-3 VALUE +1.                   
008500      05  FILLER         PIC X(23) VALUE 'VOLVO CAR PARTS     '.          
008600      05  FILLER             PIC X(74) VALUE 'W42609-001'.                
008700      05  FILLER             PIC X(07) VALUE 'DATUM'.                     
008800      05  FILLER             PIC X(16) VALUE SPACE.                       
008900                                                                          
009000   03  RUBRIK1-NL.                                                        
009100      05  FILLER             PIC S9(3) COMP-3 VALUE +1.                   
009110      05  FILLER         PIC X(23) VALUE 'VOLVO CAR PARTS     '.          
009300      05  FILLER             PIC X(74) VALUE 'W42609-001'.                
009400      05  FILLER             PIC X(07) VALUE 'DATUM'.                     
009500      05  FILLER             PIC X(16) VALUE SPACE.                       
009600                                                                          
009700   03  RUBRIK1-GB.                                                        
009800      05  FILLER             PIC S9(3) COMP-3 VALUE +1.                   
009810      05  FILLER         PIC X(23) VALUE 'VOLVO CAR PARTS     '.          
010000      05  FILLER             PIC X(74) VALUE 'W42609-001'.                
010100      05  FILLER             PIC X(07) VALUE 'DATE'.                      
010200      05  FILLER             PIC X(16) VALUE SPACE.                       
010300 01  FILLER REDEFINES RUBRIK1.                                            
010400   03  RUBRIK-1 OCCURS 3.                                                 
010500      05  RUB1-SKIP             PIC S9(03) COMP-3.                        
010600      05  FILLER                PIC X(104).                               
010700      05  RUB1-DATUM            PIC 99B99B99.                             
010800      05  FILLER                PIC X(08).                                
010900     EJECT                                                                
011000 01  RUBRIK2.                                                             
011100   03  RUBRIK2-S.                                                         
011200      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
011300      05  RUBRIK2-S             PIC X(120)                                
011400                                VALUE 'KVALITET FELFÖRDELNING'.           
011500   03  RUBRIK2-NL.                                                        
011600      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
011700      05  RUBRIK2-NL            PIC X(120)                                
011800                                VALUE 'DEFEKTEN DISTRIBUTE'.              
011900   03  RUBRIK2-GB.                                                        
012000      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
012100      05  RUBRIK2-GB            PIC X(120)                                
012200                                VALUE 'DEFECT DISTRIBUTION'.              
012300 01  FILLER REDEFINES RUBRIK2.                                            
012400   03  RUBRIK-2 OCCURS 3.                                                 
012500      05  RUB2-SKIP             PIC S9(03) COMP-3.                        
012600      05  FILLER                PIC X(120).                               
012700     EJECT                                                                
012800 01  RUBRIK3.                                                             
012900   03  RUBRIK3-S.                                                         
013000      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
013110      05  FILLER                PIC X(120)  VALUE 'LAGER'.                
013200                                                                          
013300   03  RUBRIK3-NL.                                                        
013400      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
013500      05  FILLER                PIC X(120)  VALUE 'MAGAZIJN'.             
013600                                                                          
013700   03  RUBRIK3-GB.                                                        
013800      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
013900      05  FILLER                PIC X(120)  VALUE 'WAREHOUSE'.            
014000                                                                          
014100 01  FILLER REDEFINES RUBRIK3.                                            
014200   03  RUBRIK-3 OCCURS 3.                                                 
014300      05  RUB3-SKIP             PIC S9(3) COMP-3.                         
014400      05  FILLER                PIC X(15).                                
014500      05  RUB3-IDDC             PIC XX   .                                
014600      05  FILLER                PIC X(103).                               
014700     EJECT                                                                
014800 01  RUBRIK4.                                                             
014900   03  RUBRIK4-S.                                                         
015000      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
015100      05  FILLER                PIC X(120) VALUE 'KONTR.OMRÅDE'.          
015200                                                                          
015300   03  RUBRIK4-NL.                                                        
015400      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
015500      05  FILLER                PIC X(120) VALUE 'INSPEC.AREA'.           
015600                                                                          
015700   03  RUBRIK4-GB.                                                        
015800      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
015900      05  FILLER                PIC X(120) VALUE 'INSPEC.AREA'.           
016000                                                                          
016100 01  FILLER REDEFINES RUBRIK4.                                            
016200   03  RUBRIK-4 OCCURS 3.                                                 
016300      05  RUB4-SKIP             PIC S9(03) COMP-3.                        
016400      05  FILLER                PIC X(15).                                
016500      05  RUB4-IDKVAOMR         PIC X(01).                                
016600      05  FILLER                PIC X(02).                                
016700      05  RUB4-BEKVAOMR         PIC X(10).                                
016800      05  FILLER                PIC X(92).                                
016900     EJECT                                                                
016910 01  RUBRIK45.                                                            
016920   03  RUBRIK45-S.                                                        
016930      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
016940      05  FILLER                PIC X(120) VALUE 'TORG'.                  
016950                                                                          
016960   03  RUBRIK45-NL.                                                       
016970      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
016980      05  FILLER                PIC X(120) VALUE 'AREA'.                  
016990                                                                          
016991   03  RUBRIK45-GB.                                                       
016992      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
016993      05  FILLER                PIC X(120) VALUE 'AREA'.                  
016994                                                                          
016995 01  FILLER REDEFINES RUBRIK45.                                           
016996   03  RUBRIK-45 OCCURS 3.                                                
016997      05  RUB45-SKIP            PIC S9(03) COMP-3.                        
016998      05  FILLER                PIC X(15).                                
016999      05  RUB45-IDKVATRG        PIC X(02).                                
017000      05  FILLER                PIC X(103).                               
017003     EJECT                                                                
017010 01  RUBRIK5.                                                             
017100   03  RUBRIK5-S.                                                         
017200      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
017300      05  FILLER                PIC X(120) VALUE 'PERIOD'.                
017400                                                                          
017500   03  RUBRIK5-NL.                                                        
017600      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
017700      05  FILLER                PIC X(120) VALUE 'PERIODE'.               
017800                                                                          
017900   03  RUBRIK5-GB.                                                        
018000      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
018100      05  FILLER                PIC X(120) VALUE 'PERIOD'.                
018200                                                                          
018300 01  FILLER REDEFINES RUBRIK5.                                            
018400   03  RUBRIK-5 OCCURS 3.                                                 
018500      05  RUB5-SKIP             PIC S9(03) COMP-3.                        
018600      05  FILLER                PIC X(12).                                
018700      05  RUB5-TIAARP-FOM       PIC 9(04).                                
018800      05  RUB5-STRECK           PIC X(03).                                
018900      05  RUB5-TIAARP-TOM       PIC Z(04).                                
019000      05  FILLER                PIC X(97).                                
019100     EJECT                                                                
019200 01  RUBRAD1.                                                             
019300   03  RUBRAD1-S.                                                         
019400      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
019500      05  FILLER                PIC X(87)   VALUE 'FEL'.                  
019600      05  FILLER                PIC X(08)   VALUE 'TOT'.                  
019700      05  FILLER                PIC X(13)   VALUE 'FEL-% AV'.             
019800      05  FILLER                PIC X(12)   VALUE 'FEL-% AV'.             
019900                                                                          
020000   03  RUBRAD1-NL.                                                        
020100      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
020200      05  FILLER                PIC X(86)   VALUE 'FOUT'.                 
020300      05  FILLER                PIC X(09)   VALUE 'TOTAAL'.               
020400      05  FILLER                PIC X(13)   VALUE 'DEF-% VAN'.            
020500      05  FILLER                PIC X(12)   VALUE 'DEF-% VAN'.            
020600                                                                          
020700   03  RUBRAD1-GB.                                                        
020800      05  FILLER                PIC S9(3) COMP-3 VALUE +2.                
020900      05  FILLER                PIC X(87)   VALUE 'DEF'.                  
021000      05  FILLER                PIC X(08)   VALUE 'TOT'.                  
021100      05  FILLER                PIC X(13)   VALUE 'DEF-% OF'.             
021200      05  FILLER                PIC X(12)   VALUE 'DEF-% OF'.             
021300                                                                          
021400 01  FILLER REDEFINES RUBRAD1.                                            
021500   03  RUBRAD-1 OCCURS 3.                                                 
021600      05  RUBR1-SKIP         PIC S9(03) COMP-3.                           
021700      05  FILLER             PIC X(120).                                  
021800     EJECT                                                                
021900 01  RUBRAD2.                                                             
022000   03  RUBRAD2-S.                                                         
022100      05  FILLER             PIC S9(03) COMP-3 VALUE +1.                  
022200      05  FILLER             PIC X(05)   VALUE 'KOD'.                     
022300      05  FILLER             PIC X(42)   VALUE 'FELAKTIGHET'.             
022400      05  FILLER             PIC X(24)  VALUE 'ALLVARLIGHETSGRAD'.        
022500      05  FILLER             PIC X(07)   VALUE 'ANTAL'.                   
022600      05  FILLER             PIC X(08)   VALUE 'POÄNG'.                   
022700      05  FILLER             PIC X(09)   VALUE 'POÄNG'.                   
022800      05  FILLER             PIC X(13)   VALUE 'SUMMA FEL'.               
022900      05  FILLER             PIC X(12)   VALUE 'KONTR.ART'.               
023000                                                                          
023100   03  RUBRAD2-NL.                                                        
023200      05  FILLER             PIC S9(03) COMP-3 VALUE +1.                  
023300      05  FILLER             PIC X(05)   VALUE 'KODE'.                    
023400      05  FILLER             PIC X(42)   VALUE 'SOORT DEFEKT'.            
023500      05  FILLER             PIC X(24)                                    
023600                             VALUE 'GRAAAD VAN ERNST'.                    
023700      05  FILLER             PIC X(07)   VALUE 'AANTAL'.                  
023800      05  FILLER             PIC X(08)   VALUE 'PUNTEN'.                  
023900      05  FILLER             PIC X(09)   VALUE 'PUNTEN'.                  
024000      05  FILLER             PIC X(13)   VALUE 'TOT.DEF.'.                
024100      05  FILLER             PIC X(12)   VALUE 'GEINSP.ST.'.              
024200                                                                          
024300   03  RUBRAD2-GB.                                                        
024400      05  FILLER             PIC S9(03) COMP-3 VALUE +1.                  
024500      05  FILLER             PIC X(05)   VALUE 'CODE'.                    
024600      05  FILLER             PIC X(42)   VALUE 'TYPE OF DEFECT'.          
024700      05  FILLER             PIC X(24)                                    
024800                             VALUE 'DEGREE OF SERIOUSNESS'.               
024900      05  FILLER             PIC X(07)   VALUE 'QUANT'.                   
025000      05  FILLER             PIC X(08)   VALUE 'POINTS'.                  
025100      05  FILLER             PIC X(09)   VALUE 'POINTS'.                  
025200      05  FILLER             PIC X(13)   VALUE 'TOT.DEF.'.                
025300      05  FILLER             PIC X(12)   VALUE 'INSP.PARTS'.              
025400                                                                          
025500 01  FILLER REDEFINES RUBRAD2.                                            
025600   03  RUBRAD-2 OCCURS 3.                                                 
025700      05  RUBR2-SKIP         PIC S9(03) COMP-3.                           
025800      05  FILLER             PIC X(120).                                  
025900     EJECT                                                                
026000 01  RAD.                                                                 
026100   03  RAD-SKIP              PIC S9(03) COMP-3.                           
026200   03  RAD-IDKVAFEL          PIC 9(02).                                   
026300   03  FILLER                PIC X(03)  VALUE SPACE.                      
026400   03  RAD-BEKVAFEL          PIC X(42).                                   
026500   03  RAD-BEKVAFGR          PIC X(24).                                   
026600   03  RAD-KVKVAFEL          PIC Z(04)9.                                  
026700   03  FILLER                PIC X(04)  VALUE SPACE.                      
026800   03  RAD-KVKVAFPO          PIC Z(03).                                   
026900   03  FILLER                PIC X(01)  VALUE SPACE.                      
027000   03  RAD-SUM-KVKVAFPO      PIC Z(06)9.                                  
027100   03  FILLER                PIC X(03)  VALUE SPACE.                      
027200   03  RAD-REKVAFEL          PIC Z(04)9.                                  
027300   03  FILLER                PIC X(08)  VALUE SPACE.                      
027400   03  RAD-REKVAFKA          PIC Z(04)9.9.                                
027500   03  FILLER                PIC X(08)  VALUE SPACE.                      
027600     EJECT                                                                
027700 01  SLUTRAD.                                                             
027800   03  SLUTRAD-S.                                                         
027900      05  FILLER             PIC S9(03) COMP-3 VALUE +3.                  
028000      05  FILLER             PIC X(24)                                    
028100                             VALUE 'KONTROLLERADE ARTIKLAR:'.             
028200      05  SRAD-KVART-S       PIC Z(07).                                   
028300      05  FILLER             PIC X(05) VALUE SPACE.                       
028400      05  FILLER             PIC X(20)                                    
028500                             VALUE 'FELAKTIGA ARTIKLAR:'.                 
028600      05  SRAD-KVARTFEL-S    PIC Z(07).                                   
028700      05  FILLER             PIC X(05) VALUE SPACE.                       
028800      05  FILLER             PIC X(11) VALUE 'SUMMA FEL:'.                
028900      05  SRAD-KVKVAFEL-S    PIC Z(05).                                   
029000      05  FILLER             PIC X(05) VALUE SPACE.                       
029100      05  FILLER             PIC X(08) VALUE 'FEL (%)'.                   
029200      05  SRAD-PROCENT-S     PIC Z(03).                                   
029300      05  FILLER             PIC X(20) VALUE SPACE.                       
029400                                                                          
029500   03  SLUTRAD-NL.                                                        
029600      05  FILLER             PIC S9(03) COMP-3 VALUE +3.                  
029700      05  FILLER             PIC X(16)                                    
029800                             VALUE 'GEINSP.STUKKEN:'.                     
029900      05  SRAD-KVART-NL      PIC Z(07).                                   
030000      05  FILLER             PIC X(05) VALUE SPACE.                       
030100      05  FILLER             PIC X(17)                                    
030200                             VALUE 'DEFEKTE STUKKEN:'.                    
030300      05  SRAD-KVARTFEL-NL   PIC Z(07).                                   
030400      05  FILLER             PIC X(05) VALUE SPACE.                       
030500      05  FILLER             PIC X(21)                                    
030600                             VALUE 'TOT.AANTAL DEFEKTEN:'.                
030700      05  SRAD-KVKVAFEL-NL   PIC Z(05).                                   
030800      05  FILLER             PIC X(05) VALUE SPACE.                       
030900      05  FILLER             PIC X(12) VALUE 'DEFEKTEN(%):'.              
031000      05  SRAD-PROCENT-NL    PIC Z(03).                                   
031100      05  FILLER             PIC X(17) VALUE SPACE.                       
031200                                                                          
031300   03  SLUTRAD-GB.                                                        
031400      05  FILLER             PIC S9(03) COMP-3 VALUE +3.                  
031500      05  FILLER             PIC X(16)                                    
031600                             VALUE 'INSPECTED PARTS:'.                    
031700      05  SRAD-KVART-GB      PIC Z(07).                                   
031800      05  FILLER             PIC X(05) VALUE SPACE.                       
031900      05  FILLER             PIC X(17)                                    
032000                             VALUE 'DEFECTIVE PARTS:'.                    
032100      05  SRAD-KVARTFEL-GB   PIC Z(07).                                   
032200      05  FILLER             PIC X(05) VALUE SPACE.                       
032300      05  FILLER             PIC X(15) VALUE 'TOTAL DEFECTS:'.            
032400      05  SRAD-KVKVAFEL-GB   PIC Z(05).                                   
032500      05  FILLER             PIC X(05) VALUE SPACE.                       
032600      05  FILLER             PIC X(12) VALUE 'DEFECTS(%):'.               
032700      05  SRAD-PROCENT-GB    PIC Z(03).                                   
032800      05  FILLER             PIC X(23) VALUE SPACE.                       
032900     EJECT                                                                
033000*    ---- INFIL FELFÖRDELNING RAD                                         
033100*    -COPY W4260501        -PRE IN1-                                      
033300     EJECT                                                                
033400*    ---- INFIL FELFÖRDELNING TOT                                         
033500*    -COPY W4260502        -PRE IN2-                                      
033700     EJECT                                                                
033800*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
033900     SKIP3                                                                
034000 01  DYNAMISKA-SUBPROGRAM.                                                
034100   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
034200     SKIP2                                                                
034300*    ----  PARAMETRAR TILL DATUMKORT                                      
034400                                                                          
034500 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
034600     SKIP3                                                                
034700*    -COPY WDATAREA                                                       
034900     EJECT                                                                
035000 PROCEDURE DIVISION.                                                      
035100                                                                          
035200     PERFORM A-INIT                                                       
035300                                                                          
035400     READ W42601 INTO IN-W42601   AT END                                  
035500                   MOVE 'EOF' TO INFIL-SLUT                               
035600     END-READ                                                             
035700                                                                          
035800     PERFORM UNTIL INFIL-SLUT = 'EOF'                                     
035900       IF IN-IDPTYP = 'RAD' OR 'TRG'                                      
036000          MOVE IN-W42601   TO IN1-W4260501                                
036100       ELSE                                                               
036200          MOVE IN-W42601   TO IN2-W4260502                                
036300       END-IF                                                             
036400                                                                          
036500       PERFORM B-BEHANDLING                                               
036600                                                                          
036700       READ W42601 INTO IN-W42601   AT END                                
036800                     MOVE 'EOF' TO INFIL-SLUT                             
036900       END-READ                                                           
037000     END-PERFORM                                                          
037100                                                                          
037200     PERFORM Z-FINIT                                                      
037300                                                                          
037400     MOVE ZERO            TO RETURN-CODE                                  
037500     GOBACK                                                               
037600     .                                                                    
037700     EJECT                                                                
037800 A-INIT SECTION.                                                          
037900     SKIP2                                                                
038000     OPEN  INPUT W42601                                                   
038100          OUTPUT W42609-001 W42609-001A W42609DC21 W42610DC21             
038200                                                                          
038300     MOVE 'IDAG'          TO DAT-KDDATFORM                                
038400     CALL WDATKONV     USING DAT-KDDATFORM                                
038500                             DAT-I-TIDATUM                                
038600                             DAT-O-TIDATUM                                
038700                             DAT-KDSVAR                                   
038800     .                                                                    
038900     EJECT                                                                
039000 B-BEHANDLING SECTION.                                                    
039100     SKIP2                                                                
039200                                                                          
039201     MOVE IN1-IDDC         TO WS-IDDC                                     
039210     EVALUATE TRUE                                                        
039300       WHEN  CDC-SE                                                       
039400          PERFORM BA-SKRIV-FELFORDELNING-CDC                              
039510       WHEN  SDC-NL                                                       
039600          PERFORM BB-SKRIV-FELFORDELNING-DC21                             
039700       WHEN  OTHER                                                        
039701          CONTINUE                                                        
039710     END-EVALUATE                                                         
039800     .                                                                    
039900     EJECT                                                                
040000 BA-SKRIV-FELFORDELNING-CDC SECTION.                                      
040100     SKIP2                                                                
040200     IF W-ANT-RADER > +42                                                 
040300     OR W-BRYT      = JA                                                  
040400       MOVE DAT-TIAAMMDD         TO RUB1-DATUM  (01)                      
040500       MOVE RUBRIK-1 (01)        TO W42609-001-LISTA-S-CDC                
040600       WRITE W42609-001-LISTA-S-CDC AFTER PAGE                            
040700                                                                          
040800       MOVE DAT-TIAAMMDD         TO RUB1-DATUM  (03)                      
040900       MOVE RUBRIK-1 (03)        TO W42609-001A-LISTA-GB-CDC              
041000       WRITE W42609-001A-LISTA-GB-CDC AFTER PAGE                          
041100                                                                          
041200       MOVE RUBRIK-2 (01)         TO W42609-001-LISTA-S-CDC               
041300       WRITE W42609-001-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                  
041400                                                                          
041500       MOVE RUBRIK-2 (03)         TO W42609-001A-LISTA-GB-CDC             
041600       WRITE W42609-001A-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC               
041700                                                                          
041800       MOVE IN1-IDDC            TO RUB3-IDDC (01)                         
041900       MOVE RUBRIK-3 (01)       TO W42609-001-LISTA-S-CDC                 
042000       WRITE W42609-001-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                  
042100                                                                          
042200       MOVE IN1-IDDC            TO RUB3-IDDC (03)                         
042300       MOVE RUBRIK-3 (03)       TO W42609-001A-LISTA-GB-CDC               
042400       WRITE W42609-001A-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC               
042500                                                                          
042600       IF IN1-IDKVAOMR NOT = SPACE                                        
042700          MOVE IN1-IDKVAOMR     TO RUB4-IDKVAOMR (01)                     
042800          MOVE IN1-BEKVAOMR(01) TO RUB4-BEKVAOMR (01)                     
042900          MOVE RUBRIK-4 (01)    TO W42609-001-LISTA-S-CDC                 
043000          WRITE W42609-001-LISTA-S-CDC AFTER RAD-SKIP-S-CDC               
043100                                                                          
043200          MOVE IN1-IDKVAOMR     TO RUB4-IDKVAOMR (03)                     
043300          MOVE IN1-BEKVAOMR(02) TO RUB4-BEKVAOMR (03)                     
043400          MOVE RUBRIK-4 (03)    TO W42609-001A-LISTA-GB-CDC               
043500          WRITE W42609-001A-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC            
043600       END-IF                                                             
043710                                                                          
043720       IF IN1-IDPTYP = 'TRG'                                              
043730          MOVE IN1-IDKVATRG     TO RUB45-IDKVATRG (01)                    
043750          MOVE RUBRIK-45 (01)   TO W42609-001-LISTA-S-CDC                 
043760          WRITE W42609-001-LISTA-S-CDC AFTER RAD-SKIP-S-CDC               
043770                                                                          
043780          MOVE IN1-IDKVATRG     TO RUB45-IDKVATRG (03)                    
043791          MOVE RUBRIK-45 (03)   TO W42609-001A-LISTA-GB-CDC               
043792          WRITE W42609-001A-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC            
043793       END-IF                                                             
043794                                                                          
043800       MOVE IN1-TIAARP-FOM      TO RUB5-TIAARP-FOM (01)                   
043900                                   RUB5-TIAARP-FOM (03)                   
044000       IF IN1-TIAARP-FOM = IN1-TIAARP-TOM                                 
044100          MOVE SPACE            TO RUB5-STRECK     (01)                   
044200                                   RUB5-STRECK     (03)                   
044300          MOVE ZERO             TO RUB5-TIAARP-TOM (01)                   
044400                                   RUB5-TIAARP-TOM (03)                   
044500       ELSE                                                               
044600          MOVE ' - '            TO RUB5-STRECK     (01)                   
044700                                   RUB5-STRECK     (03)                   
044800          MOVE IN1-TIAARP-TOM   TO RUB5-TIAARP-TOM (01)                   
044900                                   RUB5-TIAARP-TOM (03)                   
045000       END-IF                                                             
045100       MOVE RUBRIK-5 (01)       TO W42609-001-LISTA-S-CDC                 
045200       WRITE W42609-001-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                  
045300                                                                          
045400       MOVE RUBRIK-5 (03)       TO W42609-001A-LISTA-GB-CDC               
045500       WRITE W42609-001A-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC               
045600                                                                          
045700       MOVE RUBRAD-1 (01)       TO W42609-001-LISTA-S-CDC                 
045800       WRITE W42609-001-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                  
045900                                                                          
046000       MOVE RUBRAD-1 (03)       TO W42609-001A-LISTA-GB-CDC               
046100       WRITE W42609-001A-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC               
046200                                                                          
046300                                                                          
046400       MOVE RUBRAD-2 (01)       TO W42609-001-LISTA-S-CDC                 
046500       WRITE W42609-001-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                  
046600                                                                          
046700       MOVE RUBRAD-2 (03)       TO W42609-001A-LISTA-GB-CDC               
046800       WRITE W42609-001A-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC               
046900                                                                          
047000       MOVE +11                 TO W-ANT-RADER                            
047100     END-IF                                                               
047200                                                                          
047300     IF IN-IDPTYP = 'RAD'                                                 
047400        MOVE IN1-IDKVAFEL       TO RAD-IDKVAFEL                           
047500        MOVE IN1-BEKVAFEL (01)  TO RAD-BEKVAFEL                           
047600        MOVE IN1-BEKVAFGR (01)  TO RAD-BEKVAFGR                           
047700        MOVE IN1-KVKVAFEL       TO RAD-KVKVAFEL                           
047800        MOVE IN1-KVKVAFPO       TO RAD-KVKVAFPO                           
047900        COMPUTE W-SUM-KVKVAFPO = IN1-KVKVAFEL * IN1-KVKVAFPO              
048000        END-COMPUTE                                                       
048100        MOVE W-SUM-KVKVAFPO     TO RAD-SUM-KVKVAFPO                       
048200        MOVE IN1-REKVAFEL       TO RAD-REKVAFEL                           
048300        MOVE IN1-REKVAFKA       TO RAD-REKVAFKA                           
048400        IF W-BRYT = JA                                                    
048500           ADD  +2              TO W-ANT-RADER                            
048600           MOVE +2              TO RAD-SKIP                               
048700           MOVE NEJ             TO W-BRYT                                 
048800        ELSE                                                              
048900           ADD  +1              TO W-ANT-RADER                            
049000           MOVE +1              TO RAD-SKIP                               
049100        END-IF                                                            
049200        MOVE RAD                TO W42609-001-LISTA-S-CDC                 
049300                                                                          
049400        MOVE IN1-BEKVAFEL (02)  TO RAD-BEKVAFEL                           
049500        MOVE IN1-BEKVAFGR (02)  TO RAD-BEKVAFGR                           
049600        MOVE RAD                TO W42609-001A-LISTA-GB-CDC               
049700     ELSE                                                                 
049710       IF IN-IDPTYP = 'TRG'                                               
049720          MOVE IN1-IDKVAFEL       TO RAD-IDKVAFEL                         
049730          MOVE IN1-BEKVAFEL (01)  TO RAD-BEKVAFEL                         
049740          MOVE IN1-BEKVAFGR (01)  TO RAD-BEKVAFGR                         
049750          MOVE IN1-KVKVAFEL       TO RAD-KVKVAFEL                         
049760          MOVE IN1-KVKVAFPO       TO RAD-KVKVAFPO                         
049770          COMPUTE W-SUM-KVKVAFPO = IN1-KVKVAFEL * IN1-KVKVAFPO            
049780          END-COMPUTE                                                     
049790          MOVE W-SUM-KVKVAFPO     TO RAD-SUM-KVKVAFPO                     
049791          MOVE IN1-REKVAFEL       TO RAD-REKVAFEL                         
049792          MOVE IN1-REKVAFKA       TO RAD-REKVAFKA                         
049793          IF W-BRYT = JA                                                  
049794             ADD  +2              TO W-ANT-RADER                          
049795             MOVE +2              TO RAD-SKIP                             
049796             MOVE NEJ             TO W-BRYT                               
049797          ELSE                                                            
049798             ADD  +1              TO W-ANT-RADER                          
049799             MOVE +1              TO RAD-SKIP                             
049800          END-IF                                                          
049801          MOVE RAD                TO W42609-001-LISTA-S-CDC               
049802                                                                          
049803          MOVE IN1-BEKVAFEL (02)  TO RAD-BEKVAFEL                         
049804          MOVE IN1-BEKVAFGR (02)  TO RAD-BEKVAFGR                         
049805          MOVE RAD                TO W42609-001A-LISTA-GB-CDC             
049806        ELSE                                                              
049810          MOVE JA                 TO W-BRYT                               
049900          MOVE IN2-KVART          TO SRAD-KVART-S                         
050000                                     SRAD-KVART-GB                        
050100          MOVE IN2-KVARTFEL       TO SRAD-KVARTFEL-S                      
050200                                     SRAD-KVARTFEL-GB                     
050300          MOVE IN2-KVKVAFEL       TO SRAD-KVKVAFEL-S                      
050400                                     SRAD-KVKVAFEL-GB                     
050500          COMPUTE W-PROCENT ROUNDED =                                     
050600                 (IN2-KVARTFEL * 100) / IN2-KVART                         
050700          END-COMPUTE                                                     
050800          MOVE W-PROCENT          TO SRAD-PROCENT-S                       
050900                                     SRAD-PROCENT-GB                      
051000          ADD  +3                 TO W-ANT-RADER                          
051100          MOVE SLUTRAD-S          TO W42609-001-LISTA-S-CDC               
051200          MOVE SLUTRAD-GB         TO W42609-001A-LISTA-GB-CDC             
051210       END-IF                                                             
051300     END-IF                                                               
051400                                                                          
051500     WRITE W42609-001-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                    
051600                                                                          
051700     WRITE W42609-001A-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC                 
051800     .                                                                    
051900     EJECT                                                                
052000 BB-SKRIV-FELFORDELNING-DC21 SECTION.                                     
052100     SKIP2                                                                
052200     IF W-ANT-RADER > +42                                                 
052300     OR W-BRYT      = JA                                                  
052400       MOVE DAT-TIAAMMDD         TO RUB1-DATUM  (02)                      
052500       MOVE RUBRIK-1 (02)        TO W42609-LISTFIL-NL-DC21                
052600       WRITE W42609-LISTFIL-NL-DC21 AFTER PAGE                            
052700                                                                          
052800       MOVE DAT-TIAAMMDD         TO RUB1-DATUM  (03)                      
052900       MOVE RUBRIK-1 (03)        TO W42610-LISTFIL-GB-DC21                
053000       WRITE W42610-LISTFIL-GB-DC21 AFTER PAGE                            
053100                                                                          
053200       MOVE RUBRIK-2 (02)         TO W42609-LISTFIL-NL-DC21               
053300       WRITE W42609-LISTFIL-NL-DC21 AFTER RAD-SKIP-NL-DC21                
053400                                                                          
053500       MOVE RUBRIK-2 (03)         TO W42610-LISTFIL-GB-DC21               
053600       WRITE W42610-LISTFIL-GB-DC21 AFTER RAD-SKIP-GB-DC21                
053700                                                                          
053800       MOVE IN1-IDDC            TO RUB3-IDDC (02)                         
053900       MOVE RUBRIK-3 (02)       TO W42609-LISTFIL-NL-DC21                 
054000       WRITE W42609-LISTFIL-NL-DC21 AFTER RAD-SKIP-NL-DC21                
054100                                                                          
054200       MOVE IN1-IDDC            TO RUB3-IDDC (03)                         
054300       MOVE RUBRIK-3 (03)       TO W42610-LISTFIL-GB-DC21                 
054400       WRITE W42610-LISTFIL-GB-DC21 AFTER RAD-SKIP-GB-DC21                
054500                                                                          
054600                                                                          
054700       IF IN1-IDKVAOMR NOT = SPACE                                        
054800          MOVE IN1-IDKVAOMR     TO RUB4-IDKVAOMR (02)                     
054900          MOVE IN1-BEKVAOMR(01) TO RUB4-BEKVAOMR (02)                     
055000          MOVE RUBRIK-4 (02)    TO W42609-LISTFIL-NL-DC21                 
055100          WRITE W42609-LISTFIL-NL-DC21 AFTER RAD-SKIP-NL-DC21             
055200                                                                          
055300          MOVE IN1-IDKVAOMR     TO RUB4-IDKVAOMR (03)                     
055400          MOVE IN1-BEKVAOMR(02) TO RUB4-BEKVAOMR (03)                     
055500          MOVE RUBRIK-4 (03)    TO W42610-LISTFIL-GB-DC21                 
055600          WRITE W42610-LISTFIL-GB-DC21 AFTER RAD-SKIP-GB-DC21             
055700       END-IF                                                             
055800                                                                          
055900       MOVE IN1-TIAARP-FOM      TO RUB5-TIAARP-FOM (02)                   
056000                                   RUB5-TIAARP-FOM (03)                   
056100       IF IN1-TIAARP-FOM = IN1-TIAARP-TOM                                 
056200          MOVE SPACE            TO RUB5-STRECK     (02)                   
056300                                   RUB5-STRECK     (03)                   
056400          MOVE ZERO             TO RUB5-TIAARP-TOM (02)                   
056500                                   RUB5-TIAARP-TOM (03)                   
056600       ELSE                                                               
056700          MOVE ' - '            TO RUB5-STRECK     (02)                   
056800                                   RUB5-STRECK     (03)                   
056900          MOVE IN1-TIAARP-TOM   TO RUB5-TIAARP-TOM (02)                   
057000                                   RUB5-TIAARP-TOM (03)                   
057100       END-IF                                                             
057200       MOVE RUBRIK-5 (02)       TO W42609-LISTFIL-NL-DC21                 
057300       WRITE W42609-LISTFIL-NL-DC21 AFTER RAD-SKIP-NL-DC21                
057400                                                                          
057500       MOVE RUBRIK-5 (03)       TO W42610-LISTFIL-GB-DC21                 
057600       WRITE W42610-LISTFIL-GB-DC21 AFTER RAD-SKIP-GB-DC21                
057700                                                                          
057800       MOVE RUBRAD-1 (02)       TO W42609-LISTFIL-NL-DC21                 
057900       WRITE W42609-LISTFIL-NL-DC21 AFTER RAD-SKIP-NL-DC21                
058000                                                                          
058100       MOVE RUBRAD-1 (03)       TO W42610-LISTFIL-GB-DC21                 
058200       WRITE W42610-LISTFIL-GB-DC21 AFTER RAD-SKIP-GB-DC21                
058300                                                                          
058400                                                                          
058500       MOVE RUBRAD-2 (02)       TO W42609-LISTFIL-NL-DC21                 
058600       WRITE W42609-LISTFIL-NL-DC21 AFTER RAD-SKIP-NL-DC21                
058700                                                                          
058800       MOVE RUBRAD-2 (03)       TO W42610-LISTFIL-GB-DC21                 
058900       WRITE W42610-LISTFIL-GB-DC21 AFTER RAD-SKIP-GB-DC21                
059000                                                                          
059100       MOVE +11                 TO W-ANT-RADER                            
059200     END-IF                                                               
059300                                                                          
059400     IF IN-IDPTYP = 'RAD'                                                 
059500        MOVE IN1-IDKVAFEL       TO RAD-IDKVAFEL                           
059600        MOVE IN1-BEKVAFEL (01)  TO RAD-BEKVAFEL                           
059700        MOVE IN1-BEKVAFGR (01)  TO RAD-BEKVAFGR                           
059800        MOVE IN1-KVKVAFEL       TO RAD-KVKVAFEL                           
059900        MOVE IN1-KVKVAFPO       TO RAD-KVKVAFPO                           
060000        COMPUTE W-SUM-KVKVAFPO = IN1-KVKVAFEL * IN1-KVKVAFPO              
060100        END-COMPUTE                                                       
060200        MOVE W-SUM-KVKVAFPO     TO RAD-SUM-KVKVAFPO                       
060300        MOVE IN1-REKVAFEL       TO RAD-REKVAFEL                           
060400        MOVE IN1-REKVAFKA       TO RAD-REKVAFKA                           
060500        IF W-BRYT = JA                                                    
060600           ADD  +2              TO W-ANT-RADER                            
060700           MOVE +2              TO RAD-SKIP                               
060800           MOVE NEJ             TO W-BRYT                                 
060900        ELSE                                                              
061000           ADD  +1              TO W-ANT-RADER                            
061100           MOVE +1              TO RAD-SKIP                               
061200        END-IF                                                            
061300        MOVE RAD                TO W42609-LISTFIL-NL-DC21                 
061400                                                                          
061500        MOVE IN1-BEKVAFEL (02)  TO RAD-BEKVAFEL                           
061600        MOVE IN1-BEKVAFGR (02)  TO RAD-BEKVAFGR                           
061700        MOVE RAD                TO W42610-LISTFIL-GB-DC21                 
061800     ELSE                                                                 
061810       IF IN-IDPTYP = 'TRG'                                               
061820          MOVE IN1-IDKVAFEL       TO RAD-IDKVAFEL                         
061830          MOVE IN1-BEKVAFEL (01)  TO RAD-BEKVAFEL                         
061840          MOVE IN1-BEKVAFGR (01)  TO RAD-BEKVAFGR                         
061850          MOVE IN1-KVKVAFEL       TO RAD-KVKVAFEL                         
061860          MOVE IN1-KVKVAFPO       TO RAD-KVKVAFPO                         
061870          COMPUTE W-SUM-KVKVAFPO = IN1-KVKVAFEL * IN1-KVKVAFPO            
061880          END-COMPUTE                                                     
061890          MOVE W-SUM-KVKVAFPO     TO RAD-SUM-KVKVAFPO                     
061891          MOVE IN1-REKVAFEL       TO RAD-REKVAFEL                         
061892          MOVE IN1-REKVAFKA       TO RAD-REKVAFKA                         
061893          IF W-BRYT = JA                                                  
061894             ADD  +2              TO W-ANT-RADER                          
061895             MOVE +2              TO RAD-SKIP                             
061896             MOVE NEJ             TO W-BRYT                               
061897          ELSE                                                            
061898             ADD  +1              TO W-ANT-RADER                          
061899             MOVE +1              TO RAD-SKIP                             
061900          END-IF                                                          
061901          MOVE RAD                TO W42609-LISTFIL-NL-DC21               
061902                                                                          
061903          MOVE IN1-BEKVAFEL (02)  TO RAD-BEKVAFEL                         
061904          MOVE IN1-BEKVAFGR (02)  TO RAD-BEKVAFGR                         
061905          MOVE RAD                TO W42610-LISTFIL-GB-DC21               
061906       ELSE                                                               
061910          MOVE JA                 TO W-BRYT                               
062000          MOVE IN2-KVART          TO SRAD-KVART-NL                        
062100                                     SRAD-KVART-GB                        
062200          MOVE IN2-KVARTFEL       TO SRAD-KVARTFEL-NL                     
062300                                     SRAD-KVARTFEL-GB                     
062400          MOVE IN2-KVKVAFEL       TO SRAD-KVKVAFEL-NL                     
062500                                     SRAD-KVKVAFEL-GB                     
062600          COMPUTE W-PROCENT ROUNDED =                                     
062700                 (IN2-KVARTFEL * 100) / IN2-KVART                         
062800          END-COMPUTE                                                     
062900          MOVE W-PROCENT          TO SRAD-PROCENT-NL                      
063000                                     SRAD-PROCENT-GB                      
063100          ADD  +3                 TO W-ANT-RADER                          
063200          MOVE SLUTRAD-NL         TO W42609-LISTFIL-NL-DC21               
063300          MOVE SLUTRAD-GB         TO W42610-LISTFIL-GB-DC21               
063400       END-IF                                                             
063410     END-IF                                                               
063500                                                                          
063600     WRITE W42609-LISTFIL-NL-DC21 AFTER RAD-SKIP-NL-DC21                  
063700                                                                          
063800     WRITE W42610-LISTFIL-GB-DC21 AFTER RAD-SKIP-GB-DC21                  
063900     .                                                                    
064000     EJECT                                                                
064100 Z-FINIT SECTION.                                                         
064200     SKIP2                                                                
064300     CLOSE W42601                                                         
064310           W42609-001                                                     
064320           W42609-001A                                                    
064330           W42609DC21                                                     
064340           W42610DC21                                                     
064400     .                                                                    
064500     EJECT                                                                
