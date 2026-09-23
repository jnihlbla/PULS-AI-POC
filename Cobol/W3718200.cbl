000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3718200.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   JAN 2000.                                                
000500*    REMARKS.                                                             
000700*                SKAPAR BYTESHISTORIK TILL ON-DEMAND                      
000800*                                                                         
000900*        INDATA  W37141                                                   
001100*                                                                         
001200*                                                                         
001400     EJECT                                                                
001410                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700 FILE-CONTROL.                                                            
001800                                                                          
001900     SELECT W37141              ASSIGN TO UT-S-W37182D1.                  
002000     SELECT LISTA1              ASSIGN TO UT-S-W37182D2.                  
002100                                                                          
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400                                                                          
002500 FD  W37141                                                               
002600     RECORDING F                                                          
002700     BLOCK CONTAINS 0.                                                    
002900                                                                          
003000*01  -COPY W37109     -L.                                                 
003100                                                                          
003200 FD  LISTA1                                                               
003300     RECORDING F                                                          
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700 01  LISTPOST1                    PIC X(121).                             
003800     EJECT                                                                
003810                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004002*    -- CHECKED BY WY2000                                                 
004010 77  IDPGM               PIC X(8)        VALUE 'W3718200'.                
004200 77  JA                  PIC X               VALUE 'J'.                   
004300 77  NEJ                 PIC X               VALUE 'N'.                   
004500 77  IN-EOF              PIC X               VALUE 'N'.                   
004510 77  SIDA-SW             PIC X               VALUE 'J'.                   
004520     88  NY-SIDA                             VALUE 'J'.                   
004521 77  SPAR-DAAAVV         PIC 9(6)            VALUE ZERO.                  
004522 77  SPAR-IDDISTR-BET    PIC S9(5) COMP-3    VALUE ZERO.                  
004523 77  SPAR-IDDISTR        PIC S9(5) COMP-3    VALUE ZERO.                  
004524 77  SPAR-KDEXCHA        PIC S9(3) COMP-3    VALUE ZERO.                  
004525 77  SPAR-IDPTYP         PIC X(3)            VALUE SPACE.                 
004526 77  WS-IDARTNR-DISP     PIC 9(8).                                        
004527 77  WS-IDFKNGRP-DISP    PIC 9(4).                                        
004528 77  WS-KVANTAL-DISP     PIC 9(5).                                        
004529 77  WS-RED-IDDISTR      PIC 9(4).                                        
006100     EJECT                                                                
006110                                                                          
006200 01  INAREA.                                                              
006300*    03  -COPY W37109  -PRE IN-.                                          
006400     EJECT                                                                
006410                                                                          
006420 01  BLANKRAD                PIC X      VALUE SPACE.                      
006430                                                                          
006500 01  TITEL1.                                                              
006600     03  FILLER              PIC X(53)  VALUE                             
006700         ' W37182-001   VOLVO CAR CORPORATION, CUSTOMER SERVICE'.         
006800     03  FILLER              PIC X(27)  VALUE                             
006900         '   BYTESHISTORIK    DATUM: '.                                   
006910     03  W-DATUM-T1          PIC X(8).                                    
007000     03  FILLER              PIC X(6)   VALUE                             
007100         ' TID: '.                                                        
007200     03  W-TID-T1            PIC X(4).                                    
007800                                                                          
007810 01  RUBRIK1.                                                             
007820     03  FILLER              PIC X(9)   VALUE                             
007830         ' PERIOD: '.                                                     
007840     03  W-DAAAVV-RUB1       PIC 9(6).                                    
007850     03  FILLER              PIC X(16)  VALUE                             
007860         ' KONS.DISTRIKT: '.                                              
007870     03  W-IDDISTR-BET-RUB1  PIC X(4).                                    
007871     03  FILLER              PIC X(11)  VALUE                             
007872         ' DISTRIKT: '.                                                   
007873     03  W-IDDISTR-RUB1      PIC X(4).                                    
007874     03  FILLER              PIC X(8)   VALUE                             
007875         ' KONTO: '.                                                      
007876     03  W-KDEXCHA-RUB1      PIC X(3).                                    
007877     03  FILLER              PIC X(6)   VALUE                             
007878         ' TYP: '.                                                        
007879     03  W-IDPTYP-RUB1       PIC X(3).                                    
007910 01  RUBRIK2.                                                             
008000     03  FILLER              PIC X(49)  VALUE                             
008100         '    RAPPNR RAPPSTAT  ORDNR FKNGRP   ARTNR OBJSTAT'.             
008200     03  FILLER              PIC X(45)  VALUE                             
008300         ' BYTREF BYTKND SALDO   POÄNG  ANTAL  NOTERING'.                 
009400     EJECT                                                                
009500                                                                          
009600 01  DETALJRAD1.                                                          
009700     03  FILLER                 PIC X.                                    
010010     03  FILLER                 PIC X(2)  VALUE SPACE.                    
010100     03  RAD-IDBYTRAP           PIC X(7).                                 
010110     03  FILLER                 PIC X(4)  VALUE SPACE.                    
010200     03  RAD-KDBYTSTA-RAPP      PIC X(1).                                 
010210     03  FILLER                 PIC X(4)  VALUE SPACE.                    
010300     03  RAD-IDORDER            PIC X(7).                                 
010310     03  FILLER                 PIC X(2)  VALUE SPACE.                    
010420     03  RAD-IDFKNGRP           PIC X(4).                                 
010421     03  FILLER                 PIC X(1)  VALUE SPACE.                    
010422     03  RAD-IDARTNR            PIC X(8).                                 
010423     03  FILLER                 PIC X(4)  VALUE SPACE.                    
010424     03  RAD-KDBYTSTA-OBJ       PIC X(1).                                 
010425     03  FILLER                 PIC X(4)  VALUE SPACE.                    
010426     03  RAD-KDBYTREF           PIC X(3).                                 
010427     03  FILLER                 PIC X(7)  VALUE SPACE.                    
010428     03  RAD-FLBYTKND           PIC X(1).                                 
010429     03  FILLER                 PIC X(5)  VALUE SPACE.                    
010430     03  RAD-FLINKLBS           PIC X(1).                                 
010431     03  FILLER                 PIC X(3)  VALUE SPACE.                    
010432     03  RAD-KVPOINT            PIC X(7).                                 
010433     03  RAD-KVPOINT-TKN        PIC X(1).                                 
010434     03  FILLER                 PIC X(1)  VALUE SPACE.                    
010435     03  RAD-KVANTAL            PIC X(5).                                 
010436     03  RAD-KVANTAL-TKN        PIC X(1).                                 
010437     03  FILLER                 PIC X(1)  VALUE SPACE.                    
010438     03  RAD-TENOTE             PIC X(35).                                
012500     EJECT                                                                
012510                                                                          
012600 PROCEDURE DIVISION.                                                      
012800     PERFORM A-INITIERA                                                   
012900                                                                          
013000     PERFORM S01-LAS-INFIL                                                
013010                                                                          
013100     PERFORM UNTIL IN-EOF = JA                                            
013200       IF IN-IDPTYP NOT = 'SUP' AND                                       
013201          IN-IDPTYP NOT = 'SUT'                                           
013202         PERFORM B-BEARBETA                                               
013210         MOVE IN-DAAAVV      TO SPAR-DAAAVV                               
013220         MOVE IN-IDDISTR-BET TO SPAR-IDDISTR-BET                          
013221         MOVE IN-IDDISTR     TO SPAR-IDDISTR                              
013222         MOVE IN-KDEXCHA     TO SPAR-KDEXCHA                              
013223         MOVE IN-IDPTYP      TO SPAR-IDPTYP                               
013224       END-IF                                                             
013230                                                                          
013300       PERFORM S01-LAS-INFIL                                              
013400     END-PERFORM                                                          
013500                                                                          
013600     PERFORM Z-AVSLUTA                                                    
013610                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
013910     EJECT                                                                
014100                                                                          
014110 A-INITIERA SECTION.                                                      
014200     OPEN INPUT  W37141                                                   
014300          OUTPUT LISTA1                                                   
014310                                                                          
014500     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DATUM-T1                        
014600     MOVE FUNCTION CURRENT-DATE(9:4) TO W-TID-T1                          
015100     .                                                                    
015200     EJECT                                                                
015210                                                                          
015300 B-BEARBETA SECTION.                                                      
015600     MOVE IN-IDBYTRAP       TO RAD-IDBYTRAP                               
015700     MOVE IN-KDBYTSTA-RAPP  TO RAD-KDBYTSTA-RAPP                          
015701     MOVE IN-IDORDER        TO RAD-IDORDER                                
015702     MOVE IN-IDFKNGRP       TO WS-IDFKNGRP-DISP                           
015703     MOVE WS-IDFKNGRP-DISP  TO RAD-IDFKNGRP                               
015704     MOVE IN-IDARTNR        TO WS-IDARTNR-DISP                            
015705     MOVE WS-IDARTNR-DISP   TO RAD-IDARTNR                                
015710     MOVE IN-KDBYTSTA-OBJ   TO RAD-KDBYTSTA-OBJ                           
015800     MOVE IN-KDBYTREF       TO RAD-KDBYTREF                               
015900     MOVE IN-FLBYTKND       TO RAD-FLBYTKND                               
016000     MOVE IN-FLINKLBS       TO RAD-FLINKLBS                               
016400     MOVE IN-TENOTE         TO RAD-TENOTE                                 
016401                                                                          
016410     INSPECT RAD-IDBYTRAP REPLACING LEADING ZERO BY SPACE                 
016420     INSPECT RAD-IDORDER  REPLACING LEADING ZERO BY SPACE                 
016430     INSPECT RAD-IDFKNGRP REPLACING LEADING ZERO BY SPACE                 
016440     INSPECT RAD-IDARTNR  REPLACING LEADING ZERO BY SPACE                 
016450                                                                          
016470     IF IN-KVPOINT < ZERO                                                 
016480       COMPUTE IN-KVPOINT = IN-KVPOINT * -1                               
016490       END-COMPUTE                                                        
016500       MOVE '-'             TO RAD-KVPOINT-TKN                            
016501     ELSE                                                                 
016502       MOVE SPACE           TO RAD-KVPOINT-TKN                            
016503     END-IF                                                               
016504     MOVE IN-KVPOINT        TO RAD-KVPOINT                                
016510     INSPECT RAD-KVPOINT    REPLACING LEADING ZERO BY SPACE               
016700     IF IN-KVANTAL < ZERO                                                 
016800       COMPUTE IN-KVANTAL = IN-KVANTAL * -1                               
016900       END-COMPUTE                                                        
016910       MOVE '-'             TO RAD-KVANTAL-TKN                            
016911     ELSE                                                                 
016912       MOVE SPACE           TO RAD-KVANTAL-TKN                            
016913     END-IF                                                               
016920     MOVE IN-KVANTAL        TO WS-KVANTAL-DISP                            
016921     MOVE WS-KVANTAL-DISP   TO RAD-KVANTAL                                
016930     INSPECT RAD-KVANTAL    REPLACING LEADING ZERO BY SPACE               
017000                                                                          
017100     IF IN-DAAAVV      = SPAR-DAAAVV AND                                  
017110        IN-IDDISTR-BET = SPAR-IDDISTR-BET AND                             
017120        IN-IDDISTR     = SPAR-IDDISTR     AND                             
017130        IN-KDEXCHA     = SPAR-KDEXCHA     AND                             
017140        IN-IDPTYP      = SPAR-IDPTYP                                      
017200       MOVE NEJ             TO SIDA-SW                                    
017300     ELSE                                                                 
017400       MOVE JA              TO SIDA-SW                                    
017500     END-IF                                                               
017910                                                                          
020100     PERFORM S02-SKRIV-LISTA1                                             
020104                                                                          
020200     MOVE SPACE             TO DETALJRAD1                                 
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
027500 S01-LAS-INFIL SECTION.                                                   
027700     READ W37141 INTO INAREA                                              
027800        AT END MOVE JA TO IN-EOF                                          
027900     END-READ                                                             
028000     .                                                                    
028110                                                                          
028200 S02-SKRIV-LISTA1 SECTION.                                                
028220     IF NY-SIDA                                                           
028230       WRITE LISTPOST1 FROM TITEL1   AFTER PAGE                           
028231                                                                          
028232       WRITE LISTPOST1 FROM BLANKRAD AFTER 1                              
028233                                                                          
028234       MOVE IN-DAAAVV      TO W-DAAAVV-RUB1                               
028235       MOVE IN-IDDISTR-BET TO WS-RED-IDDISTR                              
028236       MOVE WS-RED-IDDISTR TO W-IDDISTR-BET-RUB1                          
028237       MOVE IN-IDDISTR     TO WS-RED-IDDISTR                              
028238       MOVE WS-RED-IDDISTR TO W-IDDISTR-RUB1                              
028239       MOVE IN-KDEXCHA     TO W-KDEXCHA-RUB1                              
028240       MOVE IN-IDPTYP      TO W-IDPTYP-RUB1                               
028241       INSPECT W-IDDISTR-BET-RUB1 REPLACING LEADING ZERO BY SPACE         
028242       INSPECT W-IDDISTR-RUB1     REPLACING LEADING ZERO BY SPACE         
028243       INSPECT W-KDEXCHA-RUB1     REPLACING LEADING ZERO BY SPACE         
028244       WRITE LISTPOST1 FROM RUBRIK1  AFTER 1                              
028245                                                                          
028246       WRITE LISTPOST1 FROM BLANKRAD AFTER 1                              
028247                                                                          
028250       WRITE LISTPOST1 FROM RUBRIK2  AFTER 1                              
028300     END-IF                                                               
030000                                                                          
030100     WRITE LISTPOST1 FROM DETALJRAD1 AFTER 1                              
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
032200 Z-AVSLUTA SECTION.                                                       
032400     CLOSE LISTA1 W37141                                                  
032500     .                                                                    
032600     EJECT                                                                
