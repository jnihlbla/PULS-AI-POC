000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5530700.                                                
000300 AUTHOR.         Mamatha shetty.                                          
000400 DATE-WRITTEN.   22/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        THE PROGRAM FIXES THE RIGHT LAYOUT AND CHECKS INDATA             
001000*        FOR INFREIGHT FACTOR CSV FILE.                                   
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INFREIGHT FACTOR UPDATE FILE FROM WEB-SITE                 
002500     SELECT INFILE                     ASSIGN TO W55307D1.                
002600     SKIP2                                                                
002700*          --- ADJUSTED INFREIGHT FILE TO W553D2                          
002800     SELECT UTFILE                     ASSIGN TO W55307D2.                
002900     EJECT                                                                
003000*          --- ERROR FILE                                                 
003100     SELECT W55307F                    ASSIGN TO W55307D3.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  INFILE                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000 01  CSV-RECORD            PIC X(150).                                    
004100     SKIP3                                                                
004200                                                                          
004300     SKIP3                                                                
004400 FD  UTFILE                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700 01  UT-POST               PIC X(250).                                    
004800                                                                          
004900     SKIP2                                                                
005000                                                                          
005100 FD  W55307F                                                              
005200     RECORDING       V                                                    
005300     BLOCK CONTAINS  0.                                                   
005400 01  W55307-001            PIC X(250).                                    
005500     SKIP2                                                                
005600                                                                          
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77  IDPGM                       PIC X(8)    VALUE 'W5530700'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200                                                                          
006300 77  DOTS                        PIC X       VALUE '.'.                   
006400 77  COLONS                      PIC X       VALUE ':'.                   
006500 77  COMMAS                      PIC X       VALUE ','.                   
006600 77  DOUBLE-SPACE                PIC XX      VALUE '  '.                  
006700 77  POS                         PIC S9(4)   VALUE ZERO COMP-3.           
006800 77  INTEGERS                    PIC S9(4)   VALUE ZERO COMP-3.           
006900 77  DECIMALS                    PIC S9(4)   VALUE ZERO COMP-3.           
007000 77  IND                         PIC S9(4)   VALUE ZERO COMP-3.           
007100 77  INDATA-RAKN                 PIC S9(4)   VALUE ZERO COMP-3.           
007200 77  WS-INTEGER                  PIC X(3).                                
007300 77  WS-DECIMAL                  PIC X(4).                                
007400 77  WS-CODE                     PIC 9(1).                                
007500                                                                          
007600 77  WS-IDARTNR-NUM              PIC 9(8)    VALUE ZERO.                  
007700 77  WS-IDARTNR-X                PIC X(8)    VALUE SPACE.                 
007800                                                                          
007900 77  WS-PRARTBEL-NUM             PIC S9(7)V9(2) VALUE ZERO.               
008000                                                                          
008100 77  WS-COUNTER1                 PIC S9(3)   VALUE ZERO  COMP-3.          
008200                                                                          
008300 77  INFILE-EOF-SW               PIC X       VALUE 'N'.                   
008400     88  END-OF-INFILE                       VALUE 'J'.                   
008500     88  NOT-END-OF-INFILE                   VALUE 'N'.                   
008600                                                                          
008700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008800     88  INDATA-OK                           VALUE 'J'.                   
008900     88  INDATA-FEL                          VALUE 'N'.                   
009000                                                                          
009100     EJECT                                                                
009200 01  DAGENS-DATUM                PIC X(8)    VALUE ZERO.                  
009300 01  FILLER REDEFINES DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        PIC 9(4).                                
009500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009700     EJECT                                                                
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900*                                                                         
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010400     EJECT                                                                
010500*01 -COPY WDECAREA                                                        
010600     EJECT                                                                
010700     SKIP2                                                                
010800*01 -COPY WDATAREA                                                        
010900     EJECT                                                                
011000     SKIP2                                                                
011100 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
011200     SKIP3                                                                
011300*    -COPY WZ20DATE                                                       
011400     EJECT                                                                
011500*                                                                         
011600*    --- PARAMETRAR TILL ABEND                                            
011700                                                                          
011800 77  RKOD-ABEND                  PIC S9(4) BINARY VALUE +0.               
011900 77  RKOD-SKAPA-FELMAIL          PIC S9(4) BINARY VALUE +7.               
012000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4) BINARY VALUE +16.              
012100 77  RKOD-ABEND-MED-DUMP         PIC S9(4) BINARY VALUE +1000.            
012200     SKIP2                                                                
012300 01  FELTEXT.                                                             
012400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012500     03  FELTEXT-STR             PIC X(100)  VALUE SPACE.                 
012600     EJECT                                                                
012700 01  W001-DAP.                                                            
012800     03  FILLER                  PIC X(165)  VALUE SPACE.                 
012900*    --- PARAMETRAR TILL POSTSUM                                          
013000*                                                                         
013100*01  -COPY W0005   -PRE  POSTSUM-                                         
013200     EJECT                                                                
013300 01  IN-AREA-START               PIC X(24)   VALUE                        
013400                                 'IN-AREA-START  '.                       
013500 01  IN-AREA                     PIC X(146).                              
013600                                                                          
013700 01  FILLER REDEFINES IN-AREA.                                            
013800     03 IN-COUNTRY               PIC X(3).                                
013900     03 IN-TYPE                  PIC X(3).                                
014000     03 IN-SUPPLIER              PIC X(5).                                
014100     03 IN-CODE                  PIC X(2).                                
014200     03 IN-DATE                  PIC 9(8).                                
014300     03 IN-NIF                   PIC X(8).                                
014400     03 IN-OIF                   PIC X(8).                                
014500     03 IN-PURCHASE              PIC X(3).                                
014600     03 IN-SUPPLIER-NAME         PIC X(55).                               
014700     03 IN-COUNTRY-CODE          PIC X(2).                                
014800                                                                          
014900     EJECT                                                                
015000 01  TEST-GCP-AREA               PIC X(146).                              
015100                                                                          
015200 01  FILLER REDEFINES TEST-GCP-AREA.                                      
015300     03 TEST-COUNTRY         PIC X(3).                                    
015400     03 TEST-TYPE            PIC X(15).                                   
015500     03 TEST-SUPPLIER        PIC X(15).                                   
015600     03 TEST-CODE            PIC X(15).                                   
015700     03 TEST-DATE            PIC X(15).                                   
015800     03 TEST-NIF             PIC X(15).                                   
015900     03 TEST-OIF             PIC X(15).                                   
016000     03 TEST-PURCHASE        PIC X(15).                                   
016100     03 TEST-SUPPLIER-NAME   PIC X(35).                                   
016200     03 TEST-COUNTRY-CODE    PIC X(15).                                   
016300     EJECT                                                                
016400 01  UT-AREA-START               PIC X(24)   VALUE                        
016500                                             'UT-AREA-START'.             
016600 01  UT-R18-AREA                 PIC X(250).                              
016700 01  FILLER REDEFINES UT-R18-AREA.                                        
016800     03 UT-COUNTRY             PIC X(2).                                  
016900     03 UT-TYPE                PIC X(3).                                  
017000     03 UT-SUPPLIER            PIC X(5).                                  
017100     03 UT-CODE                PIC 9(1).                                  
017200     03 UT-DATE                PIC 9(6).                                  
017300     03 UT-NIF                 PIC 9(7).                                  
017400     03 UT-OIF                 PIC X(7).                                  
017500     03 UT-PURCHASE            PIC X(3).                                  
017600                                                                          
017700 01  FELL-HEADER.                                                         
017800     03  FILLER                  PIC X(7)  VALUE 'COUNTRY'.               
017900     03  FILLER                  PIC X     VALUE ';'.                     
018000     03  FILLER                  PIC X(4)  VALUE 'TYPE'.                  
018100     03  FILLER                  PIC X     VALUE ';'.                     
018200     03  FILLER                  PIC X(8)  VALUE 'SUPPLIER'.              
018300     03  FILLER                  PIC X     VALUE ';'.                     
018400     03  FILLER             PIC X(15) VALUE 'ERR DESCRIPTION'.            
018500     03  FILLER                  PIC X     VALUE ';'.                     
018600     03  FILLER                  PIC X(4)  VALUE 'DATE'.                  
018700     03  FILLER                  PIC X     VALUE ';'.                     
018800     03  FILLER                  PIC X(13) VALUE 'NEW INFREIGHT'.         
018900     03  FILLER                  PIC X     VALUE ';'.                     
019000     03  FILLER                  PIC X(13) VALUE 'OLD INFREIGHT'.         
019100     03  FILLER                  PIC X     VALUE ';'.                     
019200     03  FILLER             PIC X(19) VALUE 'COUNTRY OF PURCHASE'.        
019300     03  FILLER                  PIC X     VALUE ';'.                     
019400     03  FILLER                  PIC X(15) VALUE 'SUPPLIER NAME'.         
019500     03  FILLER                  PIC X     VALUE ';'.                     
019600     03  FILLER                  PIC X(12) VALUE 'COUNTRY CODE'.          
019700     03  FILLER                  PIC X     VALUE ';'.                     
019800     EJECT                                                                
019900                                                                          
020000 01  FELL-DETAIL.                                                         
020100     03  FELL-COUNTRY            PIC X(2).                                
020200     03  FILLER                  PIC X     VALUE ';'.                     
020300     03  FELL-TYPE               PIC X(3).                                
020400     03  FILLER                  PIC X     VALUE ';'.                     
020500     03  FELL-SUPPLIER           PIC X(10).                               
020600     03  FILLER                  PIC X     VALUE ';'.                     
020700     03  FELL-DESCR              PIC X(45) VALUE SPACE.                   
020800     03  FILLER                  PIC X     VALUE ';'.                     
020900     03  FELL-DATE               PIC X(8)  VALUE SPACE.                   
021000     03  FILLER                  PIC X     VALUE ';'.                     
021100     03  FELL-NIF                PIC X(15).                               
021200     03  FILLER                  PIC X     VALUE ';'.                     
021300     03  FELL-OIF                PIC X(15).                               
021400     03  FILLER                  PIC X     VALUE ';'.                     
021500     03  FELL-PURCHASE           PIC X(15).                               
021600     03  FILLER                  PIC X     VALUE ';'.                     
021700     03  FELL-SUPPLIER-NAME      PIC X(35).                               
021800     03  FILLER                  PIC X     VALUE ';'.                     
021900     03  FELL-COUNTRY-CODE       PIC X(2).                                
022000     03  FILLER                  PIC X     VALUE ';'.                     
022100     EJECT                                                                
022200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
022300 01  FILLER REDEFINES TODAYS-DATE.                                        
022400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
022500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
022600     03  TODAYS-DATE-DAY         PIC 9(2).                                
022700     EJECT                                                                
022800*    --- PARAMETERS TO ABEND                                              
022900                                                                          
023000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
023100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
023200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
023300     SKIP2                                                                
023400 01  ERROR-TEXT.                                                          
023500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
023600     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
023700     EJECT                                                                
023800     EJECT                                                                
023900     SKIP2                                                                
024000     EJECT                                                                
024100 PROCEDURE DIVISION.                                                      
024200 MAIN SECTION.                                                            
024300     SKIP2                                                                
024400     PERFORM A-INIT                                                       
024500     PERFORM S01-LAES-INFILE                                              
024600     PERFORM UNTIL END-OF-INFILE                                          
024700         IF INDATA-RAKN = +1                                              
024800*          --- SKIP FIRST 1 ENTRY (HEADINGS)                              
024900           PERFORM S01-LAES-INFILE                                        
025000         ELSE                                                             
025100           PERFORM B-KOLLA-INDATA                                         
025200           IF INDATA-OK                                                   
025300             PERFORM C-SKAPA-UTFILE                                       
025400             PERFORM S11-SKRIV-UTFILE                                     
025500           END-IF                                                         
025600           PERFORM S01-LAES-INFILE                                        
025700         END-IF                                                           
025800     END-PERFORM                                                          
025900                                                                          
026000                                                                          
026100     MOVE 'S' TO POSTSUM-OPKOD                                            
026200     CALL POSTSUM USING POSTSUM-PARM                                      
026300                                                                          
026400     PERFORM Z-FINIT                                                      
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026900     OPEN INPUT  INFILE                                                   
027000     OPEN OUTPUT UTFILE                                                   
027100     OPEN OUTPUT W55307F                                                  
027200     SKIP2                                                                
027300     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
027400                                                                          
027500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027600                                                                          
027700     MOVE ZERO TO INDATA-RAKN                                             
027800                                                                          
027900     INITIALIZE UT-R18-AREA                                               
028000     .                                                                    
028100     EJECT                                                                
028200                                                                          
028300 B-KOLLA-INDATA  SECTION.                                                 
028400     MOVE JA   TO INDATA-SW                                               
028500     MOVE +1   TO IND                                                     
028600     UNSTRING IN-AREA DELIMITED BY ';' INTO                               
028700                                  IN-COUNTRY                              
028800                                  IN-TYPE                                 
028900                                  IN-SUPPLIER                             
029000                                  IN-CODE                                 
029100                                  IN-DATE                                 
029200                                  IN-NIF                                  
029300                                  IN-OIF                                  
029400                                  IN-PURCHASE                             
029500                                  IN-SUPPLIER-NAME                        
029600                                  IN-COUNTRY-CODE                         
029700                                                                          
029800     UNSTRING CSV-RECORD DELIMITED BY ';'  INTO                           
029900                                  TEST-COUNTRY                            
030000                                  TEST-TYPE                               
030100                                  TEST-SUPPLIER                           
030200                                  TEST-CODE                               
030300                                  TEST-DATE                               
030400                                  TEST-NIF                                
030500                                  TEST-OIF                                
030600                                  TEST-PURCHASE                           
030700                                  TEST-SUPPLIER-NAME                      
030800                                  TEST-COUNTRY-CODE                       
030900                                                                          
031000*    --- SUPPLIER VALIDATION                                              
031100     MOVE ZERO TO POS                                                     
031200     INSPECT TEST-SUPPLIER TALLYING POS FOR  CHARACTERS BEFORE            
031300                                     INITIAL  SPACE                       
031400     IF POS > +5                                                          
031500       MOVE NEJ TO INDATA-SW                                              
031600       MOVE 'SUPPLIER > 5 TKN :' TO FELTEXT-STR(IND:20)                   
031700       ADD +20 TO IND                                                     
031800       MOVE 'SUPPLIER  LONGER THAN 5 CHARACTER ' TO FELL-DESCR            
031900       PERFORM S05-SKAPA-FELRAD                                           
032000     ELSE                                                                 
032100         CONTINUE                                                         
032200     END-IF                                                               
032300*    --- COUNTY PURCHASE VALIDATION                                       
032400     MOVE ZERO TO POS                                                     
032500     INSPECT TEST-PURCHASE REPLACING LEADING SPACES BY ZEROS              
032600     INSPECT TEST-PURCHASE TALLYING POS FOR CHARACTERS BEFORE             
032700                                          INITIAL SPACE                   
032800     IF POS > +3                                                          
032900       MOVE NEJ TO INDATA-SW                                              
033000       MOVE 'COUNTY > 3 TKN :' TO FELTEXT-STR(IND:20)                     
033100       ADD +20 TO IND                                                     
033200       MOVE 'COUNTY  LONGER THAN 3 CHARACTERS ' TO FELL-DESCR             
033300       PERFORM S05-SKAPA-FELRAD                                           
033400     ELSE                                                                 
033500       IF TEST-PURCHASE(1:POS) NOT NUMERIC                                
033600         MOVE NEJ TO INDATA-SW                                            
033700         MOVE 'IN-PURCHASE NOT NUM :' TO FELTEXT-STR(IND:20)              
033800         ADD +20 TO IND                                                   
033900         MOVE 'COUNTY PURCHASE IS NOT NUMERIC ' TO FELL-DESCR             
034000         PERFORM S05-SKAPA-FELRAD                                         
034100       ELSE                                                               
034200         CONTINUE                                                         
034300       END-IF                                                             
034400     END-IF                                                               
034500                                                                          
034600*    --- OLD INFREIGHT FACTOR VALIDATION                                  
034700                                                                          
034800     MOVE ZERO TO POS                                                     
034900     INSPECT TEST-OIF      REPLACING LEADING SPACES BY ZEROS              
035000     INSPECT TEST-OIF        TALLYING POS FOR CHARACTERS                  
035100                           BEFORE INITIAL SPACE                           
035200     UNSTRING TEST-OIF DELIMITED BY DOTS                                  
035300                             INTO WS-INTEGER                              
035400                                  WS-DECIMAL                              
035500                                                                          
035600       IF POS > +8                                                        
035700         MOVE NEJ TO INDATA-SW                                            
035800         MOVE 'OIF  > 8 TKN :' TO FELTEXT-STR(IND:18)                     
035900         ADD +18 TO IND                                                   
036000         MOVE 'OLD INFREIGHT IS OVER MAX.LENGTH ' TO FELL-DESCR           
036100         PERFORM S05-SKAPA-FELRAD                                         
036200       ELSE                                                               
036300         IF POS > +0                                                      
036400           MOVE ZERO TO INTEGERS                                          
036500                        DECIMALS                                          
036600           INSPECT WS-INTEGER TALLYING INTEGERS FOR CHARACTERS            
036700                 BEFORE INITIAL SPACE                                     
036800           INSPECT WS-DECIMAL TALLYING DECIMALS FOR CHARACTERS            
036900                 BEFORE INITIAL SPACE                                     
037000           IF INTEGERS > +0 OR DECIMALS > +0                              
037100             IF WS-INTEGER(1:INTEGERS)  NOT NUMERIC                       
037200               MOVE NEJ TO INDATA-SW                                      
037300               MOVE 'OLD INFREIGHT NOT NUM :'                             
037400                                            TO FELTEXT-STR(IND:18)        
037500               ADD +18 TO IND                                             
037600               MOVE 'OLD INFREIGHT INTEGER PART IS NOT NUMERIC'           
037700                       TO FELL-DESCR                                      
037800               PERFORM S05-SKAPA-FELRAD                                   
037900             ELSE                                                         
038000             IF WS-DECIMAL(1:DECIMALS)  NOT NUMERIC                       
038100                 MOVE NEJ TO INDATA-SW                                    
038200                 MOVE 'OLD INFREIGHT NOT NUM :'                           
038300                                            TO FELTEXT-STR(IND:18)        
038400                 ADD +18 TO IND                                           
038500                 MOVE 'OLD INFREIGHT DECIMAL PART IS NOT NUMERIC '        
038600                      TO FELL-DESCR                                       
038700                 PERFORM S05-SKAPA-FELRAD                                 
038800               ELSE                                                       
038900               IF INTEGERS > 3 OR DECIMALS > 4                            
039000                 MOVE NEJ TO INDATA-SW                                    
039100                 MOVE 'OLD INFREIGHT FACTOR NOT OK:'                      
039200                                         TO FELTEXT-STR(IND:16)           
039300                 ADD +16 TO IND                                           
039400                 MOVE 'OLD INFREIGHT FACTOR NOT OK FORMAT '               
039500                                                     TO FELL-DESCR        
039600                 PERFORM S05-SKAPA-FELRAD                                 
039700               END-IF                                                     
039800             END-IF                                                       
039900           END-IF                                                         
040000           ELSE                                                           
040100*            --- Old infreight factor  has no integers                    
040200             MOVE NEJ TO INDATA-SW                                        
040300             MOVE 'OLD INFREIGHT FACTOR HAS NO INTEGERS:'                 
040400                                   TO FELTEXT-STR(IND:25)                 
040500             ADD +25 TO IND                                               
040600             MOVE 'OLD INFREIGHT HAS NO INTEGERS ' TO FELL-DESCR          
040700             PERFORM S05-SKAPA-FELRAD                                     
040800           END-IF                                                         
040900         ELSE                                                             
041000           MOVE NEJ TO INDATA-SW                                          
041100           MOVE 'OLD INFREIGHT FACTOR MISSING :'                          
041200                                          TO FELTEXT-STR(IND:18)          
041300           ADD +18 TO IND                                                 
041400           MOVE 'NO OLD INFREIGHT FACTOR DECLARED ' TO FELL-DESCR         
041500           PERFORM S05-SKAPA-FELRAD                                       
041600         END-IF                                                           
041700       END-IF                                                             
041800                                                                          
041900*    --- NEW INFREIGHT FACTOR VALIDATION                                  
042000                                                                          
042100     MOVE ZERO TO POS                                                     
042200     INSPECT TEST-NIF      REPLACING LEADING SPACES BY ZEROS              
042300     INSPECT TEST-NIF        TALLYING POS FOR CHARACTERS                  
042400                           BEFORE INITIAL SPACE                           
042500     UNSTRING TEST-NIF DELIMITED BY DOTS                                  
042600                             INTO WS-INTEGER                              
042700                                  WS-DECIMAL                              
042800                                                                          
042900       IF POS > +8                                                        
043000         MOVE NEJ TO INDATA-SW                                            
043100         MOVE 'NIF  > 8 TKN :' TO FELTEXT-STR(IND:18)                     
043200         ADD +18 TO IND                                                   
043300         MOVE 'NEW INFREIGHT IS OVER MAX.LENGTH ' TO FELL-DESCR           
043400         PERFORM S05-SKAPA-FELRAD                                         
043500       ELSE                                                               
043600         IF POS > +0                                                      
043700           MOVE ZERO TO INTEGERS                                          
043800                        DECIMALS                                          
043900           INSPECT WS-INTEGER TALLYING INTEGERS FOR CHARACTERS            
044000                 BEFORE INITIAL SPACE                                     
044100           INSPECT WS-DECIMAL TALLYING DECIMALS FOR CHARACTERS            
044200                 BEFORE INITIAL SPACE                                     
044300           IF INTEGERS > +0 OR DECIMALS > +0                              
044400             IF WS-INTEGER(1:INTEGERS) NOT NUMERIC                        
044500               MOVE NEJ TO INDATA-SW                                      
044600               MOVE 'NEW INFREIGHT NOT NUM :'                             
044700                                            TO FELTEXT-STR(IND:18)        
044800               ADD +18 TO IND                                             
044900               MOVE 'NEW INFREIGHT INTEGER PART IS NOT NUMERIC'           
045000                       TO FELL-DESCR                                      
045100               PERFORM S05-SKAPA-FELRAD                                   
045200             ELSE                                                         
045300             IF WS-DECIMAL(1:DECIMALS)  NOT NUMERIC                       
045400                 MOVE NEJ TO INDATA-SW                                    
045500                 MOVE 'NEW INFREIGHT NOT NUM :'                           
045600                                            TO FELTEXT-STR(IND:18)        
045700                 ADD +18 TO IND                                           
045800                 MOVE 'NEW INFREIGHT DECIMAL PART IS NOT NUMERIC '        
045900                      TO FELL-DESCR                                       
046000                 PERFORM S05-SKAPA-FELRAD                                 
046100               ELSE                                                       
046200               IF INTEGERS > 3 OR DECIMALS > 4                            
046300                 MOVE NEJ TO INDATA-SW                                    
046400                 MOVE 'NEW INFREIGHT FACTOR NOT OK:'                      
046500                                         TO FELTEXT-STR(IND:16)           
046600                 ADD +16 TO IND                                           
046700                 MOVE 'NEW INFREIGHT FACTOR NOT OK FORMAT '               
046800                                                     TO FELL-DESCR        
046900                 PERFORM S05-SKAPA-FELRAD                                 
047000               END-IF                                                     
047100             END-IF                                                       
047200           END-IF                                                         
047300           ELSE                                                           
047400*            --- NEW INFREIGHT FACTOR  HAS NO INTEGERS                    
047500             MOVE NEJ TO INDATA-SW                                        
047600             MOVE 'NEW INFREIGHT FACTOR HAS NO INTEGERS:'                 
047700                                   TO FELTEXT-STR(IND:25)                 
047800             ADD +25 TO IND                                               
047900             MOVE 'NEW INFREIGHT HAS NO INTEGERS ' TO FELL-DESCR          
048000             PERFORM S05-SKAPA-FELRAD                                     
048100           END-IF                                                         
048200         END-IF                                                           
048300       END-IF                                                             
048400                                                                          
048500                                                                          
048600*    --- INFREIGHT FACTOR DATE                                            
048700     MOVE ZERO TO POS                                                     
048800     INSPECT TEST-DATE TALLYING POS FOR CHARACTERS BEFORE                 
048900                                            INITIAL '  '                  
049000     IF POS NOT = +8                                                      
049100       MOVE NEJ TO INDATA-SW                                              
049200       MOVE 'DATE EJ 8 TKN :' TO FELTEXT-STR(IND:19)                      
049300       ADD +19 TO IND                                                     
049400       MOVE 'DATE HAS NOT 8 DIGITS YYYYMMDD ' TO FELL-DESCR               
049500       PERFORM S05-SKAPA-FELRAD                                           
049600     ELSE                                                                 
049700       IF TEST-DATE(1:POS)  NOT NUMERIC                                   
049800         MOVE NEJ TO INDATA-SW                                            
049900         MOVE 'DATE NOT NUM :' TO FELTEXT-STR(IND:17)                     
050000         ADD +17 TO IND                                                   
050100         MOVE 'DATE IS NOT NUMERIC ' TO FELL-DESCR                        
050200         PERFORM S05-SKAPA-FELRAD                                         
050300       ELSE                                                               
050400         IF TEST-DATE(1:2) NOT = 20                                       
050500           MOVE NEJ TO INDATA-SW                                          
050600           MOVE 'DATE FEL FORM :' TO FELTEXT-STR(IND:19)                  
050700           ADD +19 TO IND                                                 
050800           MOVE 'YEAR MUST BE SPECIFIED WITH 4 DIGITS '                   
050900                TO FELL-DESCR                                             
051000           PERFORM S05-SKAPA-FELRAD                                       
051100         ELSE                                                             
051200         MOVE TEST-DATE           TO DATE-TIDATE                          
051300         MOVE 'YYYYMMDD'          TO DATE-KDDATFMT                        
051400         CALL WZ20DATE USING DATE-WZ20DATE                                
051500             IF DATE-KDRC = 0                                             
051600              IF TEST-DATE > DAGENS-DATUM                                 
051700                CONTINUE                                                  
051800              ELSE                                                        
051900                MOVE NEJ TO INDATA-SW                                     
052000                MOVE 'DATE <= DAGENS:' TO FELTEXT-STR(IND:19)             
052100                ADD +19 TO IND                                            
052200                MOVE 'DATE IS NOT GREATER THAN TODAYS DATE '              
052300                     TO FELL-DESCR                                        
052400                PERFORM S05-SKAPA-FELRAD                                  
052500              END-IF                                                      
052600            ELSE                                                          
052700              MOVE NEJ TO INDATA-SW                                       
052800              MOVE 'DATE EJ DATUM :' TO FELTEXT-STR(IND:19)               
052900              ADD +19 TO IND                                              
053000              MOVE 'NOT A VALID DATE FORMAT ' TO FELL-DESCR               
053100              PERFORM S05-SKAPA-FELRAD                                    
053200            END-IF                                                        
053300         END-IF                                                           
053400       END-IF                                                             
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800                                                                          
053900 C-SKAPA-UTFILE  SECTION.                                                 
054000     MOVE IN-COUNTRY(2:2)   TO UT-COUNTRY                                 
054100     MOVE 'R18'             TO UT-TYPE                                    
054200     MOVE IN-SUPPLIER       TO UT-SUPPLIER                                
054300     MOVE IN-CODE(1:1)      TO UT-CODE                                    
054400     MOVE IN-DATE(3:6)      TO UT-DATE                                    
054500     MOVE TEST-NIF(1:3)     TO UT-NIF(1:3)                                
054600     MOVE TEST-NIF(5:4)     TO UT-NIF(4:4)                                
054700     MOVE TEST-OIF(1:3)     TO UT-OIF(1:3)                                
054800     MOVE TEST-OIF(5:4)     TO UT-OIF(4:4)                                
054900     MOVE TEST-PURCHASE    TO UT-PURCHASE                                 
055000     .                                                                    
055100     EJECT                                                                
055200                                                                          
055300 Z-FINIT SECTION.                                                         
055400     CLOSE INFILE                                                         
055500           UTFILE                                                         
055600           W55307F                                                        
055700     SKIP2                                                                
055800     .                                                                    
055900     EJECT                                                                
056000                                                                          
056100 S01-LAES-INFILE  SECTION.                                                
056200     READ INFILE INTO IN-AREA                                             
056300     AT END                                                               
056400        MOVE HIGH-VALUE TO IN-AREA                                        
056500        SET END-OF-INFILE TO TRUE                                         
056600                                                                          
056700     NOT AT END                                                           
056800        MOVE 'INFILE'   TO POSTSUM-FDNAMN                                 
056900        MOVE 'W55307D1' TO POSTSUM-DDNAMN2                                
057000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
057100        CALL POSTSUM USING POSTSUM-PARM                                   
057200                                                                          
057300        ADD +1 TO INDATA-RAKN                                             
057400     END-READ                                                             
057500     .                                                                    
057600     EJECT                                                                
057700                                                                          
057800 S11-SKRIV-UTFILE SECTION.                                                
057900     WRITE UT-POST FROM UT-R18-AREA                                       
058000                                                                          
058100     MOVE 'UT '         TO POSTSUM-TRANSTYP                               
058200     MOVE 'W55307'      TO POSTSUM-FDNAMN                                 
058300     MOVE 'W55307D2'    TO POSTSUM-DDNAMN2                                
058400     CALL POSTSUM USING POSTSUM-PARM                                      
058500     .                                                                    
058600     EJECT                                                                
058700                                                                          
058800 S05-SKAPA-FELRAD SECTION.                                                
058900     MOVE TEST-COUNTRY(2:2)          TO FELL-COUNTRY                      
059000     MOVE 'R18'                      TO FELL-TYPE                         
059100     MOVE TEST-SUPPLIER              TO FELL-SUPPLIER                     
059200     MOVE TEST-DATE                  TO FELL-DATE                         
059300     MOVE TEST-NIF                   TO FELL-NIF                          
059400     MOVE TEST-OIF                   TO FELL-OIF                          
059500     MOVE TEST-PURCHASE              TO FELL-PURCHASE                     
059600     MOVE TEST-SUPPLIER-NAME         TO FELL-SUPPLIER-NAME                
059700     MOVE TEST-COUNTRY-CODE          TO FELL-COUNTRY-CODE                 
059800                                                                          
059900     PERFORM S06-SKRIV-ERRFILE                                            
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300 S06-SKRIV-ERRFILE     SECTION.                                           
060400     IF WS-COUNTER1 = ZERO                                                
060500       MOVE  ' ¤DAPW55307-001' TO W001-DAP                                
060600       WRITE W55307-001   FROM W001-DAP                                   
060700       MOVE  ' ¤DAPW553'       TO W001-DAP                                
060800       WRITE W55307-001   FROM W001-DAP                                   
060900       WRITE W55307-001   FROM FELL-HEADER                                
061000     END-IF                                                               
061100     WRITE W55307-001   FROM FELL-DETAIL                                  
061200     ADD +1 TO WS-COUNTER1                                                
061300     .                                                                    
061400     SKIP3                                                                
