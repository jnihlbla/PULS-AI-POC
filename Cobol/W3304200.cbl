000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3304200.                                                 
000400 AUTHOR.        PETER DAHLÖF.                                             
000500 DATE-WRITTEN.  DECEMBER 1989.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*    LÄSER FILEN W33041 RESULTAT OCH MATCHAR MED W33031-FILEN             
001000*    SAMTLIGA URVAL. PLOCKAR ENBART P-URVALEN OCH SKRIVER FIL             
001100*    W33043. FÖR ATT KUNNA BERÄKNA NIVÅER SKAPAS YTTERLIGARE              
001200*    ETT ANTAL POSTER BEROENDE PÅ VAD SOM STÅR I KDNIVA PÅ UR-            
001300*    VALS FILEN. EX. URVALS FILEN VISAR ATT KDNIVA = 4. DÅ GÖRS           
001400*    FÖLJANDE. RESULTATFILEN HAR RESULTAT PÅ ARTIKELNIVÅ                  
001500*    DESSA SKALL LÄGGAS IHOP TILL EN FUNKTIONSGRUPPSNIVÅ. NÄR             
001600*    DENNA FUNKTIONGRP (TEX 1142) SKALL SKRIVAS SKRIVS SAMMA              
001700*    RESULTAT ÄVEN PÅ FUNKTINSGRP 1140 1100 1000 SAMT 99999.              
001800*    DEN SISTA (99999) ANVÄNDS FÖR ATT RÄKNA FRAM EN PRODUKT-             
001900*    TOTAL. SAMTLIGA TOTALER BERÄKNAS I PROGRAM W33044.                   
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*    --- INFILER:                                                         
002800     SELECT W33031S                      ASSIGN TO W33042D1.              
002900     SELECT W33041S                      ASSIGN TO W33042D2.              
003000     SELECT SORTFIL                      ASSIGN TO W33042DS.              
003100*    --- UTFILER:                                                         
003200     SELECT W33043                       ASSIGN TO W33042D3.              
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W33031S                                                              
003900     LABEL RECORD   STANDARD                                              
004000     RECORDING      V                                                     
004100     BLOCK CONTAINS 0.                                                    
004200     SKIP2                                                                
004300*01  POST -COPY W3303103  -L -PRE I331-.                                  
004500     SKIP2                                                                
004600*01  POST -COPY W3303102  -L -PRE I231-.                                  
004800     SKIP2                                                                
004900*01  POST -COPY W3303104  -L -PRE I431-.                                  
005100     EJECT                                                                
005200 FD  W33041S                                                              
005300     LABEL RECORD   STANDARD                                              
005400     RECORDING      F                                                     
005500     BLOCK CONTAINS 0.                                                    
005600     SKIP2                                                                
005700*01  POST -COPY W33041  -L -PRE I41-.                                     
005900     EJECT                                                                
006000 SD  SORTFIL                                                              
006100     RECORDING      V                                                     
006200     SKIP2                                                                
006300 01  SORTERAD-POST.                                                       
006400   03  SORT-IDUSER                 PIC X(8).                              
006500   03  SORT-DAREGDAT               PIC 9(8).                              
006600   03  SORT-TIREGTID               PIC 9(7).                              
006700   03  SORT-PRODSL                 PIC 9(3).                              
006800   03  SORT-FKNGRP                 PIC 9(5).                              
006900   03  SORT-BEGREPP                PIC 9(5).                              
007000*03  POST -COPY W3303103  -L.                                             
007200     SKIP2                                                                
007300 01  SORTERAD-POST231.                                                    
007400   03  FILLER                   PIC X(36).                                
007500*03  POST -COPY W3303102  -L.                                             
007700     SKIP2                                                                
007800 01  SORTERAD-POST41.                                                     
007900      03  FILLER                   PIC X(36).                             
008000*03  POST -COPY W33041  -L.                                               
008200     EJECT                                                                
008300 FD  W33043                                                               
008400     LABEL RECORD   STANDARD                                              
008500     RECORDING      F                                                     
008600     BLOCK CONTAINS 0.                                                    
008700     SKIP2                                                                
008800*01  POST -COPY W33045  -L -PRE U43-.                                     
009000     EJECT                                                                
009100 WORKING-STORAGE SECTION.                                                 
009110                                                                          
009120*    -- CHECKED BY WY2000                                                 
009130                                                                          
009200     SKIP2                                                                
009300 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3304200'.            
009400 77  JA                          PIC X(1)    VALUE 'J'.                   
009500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
009600 77  I41-EOF                     PIC X(1)    VALUE 'N'.                   
009700 77  I31-EOF                     PIC X(1)    VALUE 'N'.                   
009800 77  SORT-EOF                    PIC X(1)    VALUE 'N'.                   
009910 77  ADDERA-PS                   PIC X(1)    VALUE 'N'.                   
009920 77  IX                          PIC S9(4)   VALUE +1  COMP SYNC.         
010000 77  ABENDKOD                    PIC S9(4)   VALUE +16 COMP SYNC.         
010100 77  SPAR-BEGREPP                PIC 9(5).                                
010200 77  SPAR-PRODSL                 PIC 9(3).                                
010300 77  WS-KDNIVA                   PIC 9(1) VALUE ZERO.                     
010400 77  WS-IDFKNGRP                 PIC 9(5).                                
010500                                                                          
010510                                                                          
010520 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
010530 01  WS-SEKTION              PIC X(30)   VALUE SPACE.                     
010540 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
010550 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
010560 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
010570 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
010580                                                                          
010600 01  SPAR-FKNGRP.                                                         
010700   03  SPAR-IDFKNGRP             PIC 9(5).                                
010800 01  FILLER REDEFINES SPAR-FKNGRP.                                        
010900   03  FILLER                    PIC 9(1).                                
011000   03  SPAR-2-FOERSTA            PIC 9(1).                                
011100   03  FILLER                    PIC 9(3).                                
011200 01  FILLER REDEFINES SPAR-FKNGRP.                                        
011300   03  FILLER                    PIC 9(1).                                
011400   03  SPAR-3-FOERSTA            PIC 9(2).                                
011500   03  FILLER                    PIC 9(2).                                
011600 01  FILLER REDEFINES SPAR-FKNGRP.                                        
011700   03  FILLER                    PIC 9(1).                                
011800   03  SPAR-4-FOERSTA            PIC 9(3).                                
011900   03  FILLER                    PIC 9(1).                                
012000 01  FILLER REDEFINES SPAR-FKNGRP.                                        
012100   03  FILLER                    PIC 9(2).                                
012200   03  SPAR-FKNGRP-POS-3         PIC 9(1).                                
012300   03  FILLER                    PIC 9(2).                                
012400 01  FILLER REDEFINES SPAR-FKNGRP.                                        
012500   03  FILLER                    PIC 9(3).                                
012600   03  SPAR-FKNGRP-POS-4         PIC 9(1).                                
012700   03  FILLER                    PIC 9(1).                                
012800     EJECT                                                                
012900 01  DYNAMISKA-SUBPROGRAM.                                                
013000     SKIP1                                                                
013100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
013200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013300     SKIP2                                                                
013400*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
013500*                                                                         
013600 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
013700*01  -COPY W0005 -PRE  POSTSUM-.                                          
013900     EJECT                                                                
014000 01  FILLER                       PIC X(16)  VALUE 'I31-FILEN'.           
014100*                                                                         
014200*     FIL W33031                                                          
014300 01  I31-AREA.                                                            
014400   03  I31-IDUSER                     PIC X(8).                           
014500   03  I31-DAREGDAT                   PIC 9(8).                           
014600   03  I31-TIREGTID                   PIC S9(7) COMP-3.                   
014700   03  FILLER                         PIC X(8).                           
014800   03  I31-IDPTYP                     PIC X(3).                           
014900   03  I31-IDGTYP                     PIC S9(1) COMP-3.                   
015000   03  I31-IDTRANS                    PIC X(4).                           
015100   03  I31-IDKONCNR OCCURS 8 TIMES    PIC S9(3) COMP-3.                   
015200   03  I31-MARKNADS-GRP OCCURS 8 TIMES.                                   
015300     05  I31-KDMARK-BUDG-FOM          PIC S9(3) COMP-3.                   
015400     05  I31-KDMARK-BUDG-TOM          PIC S9(3) COMP-3.                   
015500   03  I31-DISTRIKT-GRP OCCURS 4 TIMES.                                   
015600     05  I31-IDDISTR-FOM              PIC S9(5) COMP-3.                   
015700     05  I31-IDDISTR-TOM              PIC S9(5) COMP-3.                   
015800   03  FILLER                         PIC X(2560).                        
015900*01  AREA  -COPY W3303103  -PRE I331- -RED I31-AREA.                      
016100    EJECT                                                                 
016200*01  AREA  -COPY W3303102  -PRE I231- -RED I31-AREA.                      
016400    EJECT                                                                 
016500 01  FILLER                       PIC X(16)  VALUE 'I41-FILEN'.           
016600*01  AREA  -COPY W33041  -PRE I41-.                                       
016800    EJECT                                                                 
016900 01  FILLER                       PIC X(16)  VALUE                        
017000                                          'WS-SORTERAD-AREA'.             
017100 01  WS-SORTERAD-AREA.                                                    
017200   03  WS-SORT-IDUSER              PIC X(8).                              
017300   03  WS-SORT-DAREGDAT            PIC 9(8).                              
017400   03  WS-SORT-TIREGTID            PIC 9(7).                              
017500   03  WS-SORT-PRODSL              PIC 9(3).                              
017600   03  WS-SORT-FKNGRP              PIC 9(5).                              
017700   03  WS-SORT-BEGREPP             PIC 9(5).                              
017800   03  WS-CTEXT.                                                          
017900     05  FILLER                    PIC X(31).                             
018000     05  WS-SORT-IDGTYP            PIC S9(1) COMP-3.                      
018100     05  WS-SORT-IDTRANS           PIC X(4).                              
018200     05  FILLER                    PIC X(2572).                           
018300*03  AREA -COPY W33041  -PRE SORT41- -RED WS-CTEXT.                       
018500     EJECT                                                                
018600*03  AREA -COPY W3303103  -PRE SORT331- -RED WS-CTEXT.                    
018800     EJECT                                                                
018900*03  AREA -COPY W3303102  -PRE SORT231- -RED WS-CTEXT.                    
019100     EJECT                                                                
019200 01  FILLER                       PIC X(16)  VALUE 'UTFILER'.             
019300*                                                                         
019400*     FIL W33043                                                          
019500*01  AREA  -COPY W33045  -PRE U43-.                                       
019700     EJECT                                                                
019800*01  AREA  -COPY W33045  -PRE 43NTLZD-.                                   
020000     EJECT                                                                
020100 PROCEDURE DIVISION.                                                      
020200    SKIP2                                                                 
020300 STYR SECTION.                                                            
020400* BÖRJA MED 2 SORTAR I PROCEDUREN                                         
020500     PERFORM A-INIT                                                       
020600     SORT SORTFIL                                                         
020700        ASCENDING KEY SORT-IDUSER                                         
020800                      SORT-DAREGDAT                                       
020900                      SORT-TIREGTID                                       
021000                      SORT-PRODSL                                         
021100                      SORT-FKNGRP                                         
021200                      SORT-BEGREPP                                        
021300        INPUT PROCEDURE  B-PLOCKA-P-URVAL                                 
021400        OUTPUT PROCEDURE C-BEHANDLA-OCH-SKRIV                             
021500     IF SORT-RETURN = ZERO                                                
021600        PERFORM Z-FINIT                                                   
021700        MOVE ZERO TO RETURN-CODE                                          
021800        GOBACK                                                            
021900     ELSE                                                                 
022000        DISPLAY 'FEL I SORTERINGEN'                                       
022100        CALL ABEND USING ABENDKOD                                         
022200     END-IF                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022510     MOVE 'A-INIT '       TO WS-SEKTION                                   
022600     SKIP2                                                                
022700     OPEN INPUT  W33031S                                                  
022800                 W33041S                                                  
022900     OPEN OUTPUT W33043                                                   
023000     MOVE PROGRAM-NAMN               TO POSTSUM-PROGNAMN                  
023100*    MOVE 274                        TO SORT-MODE-SIZE                    
023200     INITIALIZE 43NTLZD-AREA                                              
023300     MOVE 43NTLZD-AREA               TO U43-AREA                          
023400     .                                                                    
023500     EJECT                                                                
023600 B-PLOCKA-P-URVAL    SECTION.                                             
023610     MOVE 'B-PLOCKA-P-URVAL ' TO WS-SEKTION                               
023700     SKIP2                                                                
023800     PERFORM S01-LAS-31-FIL                                               
023900     PERFORM S02-LAS-41-FIL                                               
024000     PERFORM UNTIL I41-EOF = JA AND                                       
024100     I31-EOF = JA                                                         
024200        IF I41-EOF = NEJ                                                  
024300           IF I41-IDUSER   = I31-IDUSER   AND                             
024400           I41-DAREGDAT    = I31-DAREGDAT AND                             
024500           I41-TIREGTID    = I31-TIREGTID AND                             
024600           I31-IDPTYP   = 'P1 ' OR 'PLV' OR 'PPV'                         
024700              PERFORM BA-RELEASE-RESULTAT                                 
024800              PERFORM BB-RELEASE-P1-POST                                  
024900           ELSE                                                           
024910              IF I31-IDPTYP   = 'P1 ' OR 'PLV' OR 'PPV'                   
025100                 PERFORM BB-RELEASE-P1-POST                               
025200              END-IF                                                      
025300           END-IF                                                         
025400        ELSE                                                              
025410           IF I31-IDPTYP   = 'P1 ' OR 'PLV' OR 'PPV'                      
025600              PERFORM BB-RELEASE-P1-POST                                  
025700           END-IF                                                         
025800        END-IF                                                            
025900        PERFORM S01-LAS-31-FIL                                            
026000     END-PERFORM                                                          
026100     .                                                                    
026200     EJECT                                                                
026300 BA-RELEASE-RESULTAT    SECTION.                                          
026310     MOVE 'BA-RELEASE-RESULTAT' TO WS-SEKTION                             
026400     SKIP2                                                                
026500     MOVE +1                         TO IX                                
026520     IF I231-IDTRANS = '3202'                                             
026530        IF I231-KDSVAR = JA                                               
026540           MOVE JA TO ADDERA-PS                                           
026550        ELSE                                                              
026560           MOVE NEJ TO ADDERA-PS                                          
026570        END-IF                                                            
026580     ELSE                                                                 
026590        MOVE NEJ TO ADDERA-PS                                             
026591     END-IF                                                               
026600     IF I31-IDGTYP = +1                                                   
026700        PERFORM BAA-SORTERA-PA-DISTR                                      
026800     ELSE                                                                 
026900        IF I31-IDGTYP = +2                                                
027000           PERFORM BAB-SORTERA-PA-KONCNR                                  
027100        ELSE                                                              
027200           IF I31-IDGTYP = +3                                             
027300              PERFORM BAC-SORTERA-PA-MARKN                                
027400           ELSE                                                           
027500              PERFORM BAD-SORTERA-PA-WORLD-WIDE                           
027600           END-IF                                                         
027700        END-IF                                                            
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 BAA-SORTERA-PA-DISTR       SECTION.                                      
028110     MOVE 'BAA-SORTERA-PA-DISTR' TO WS-SEKTION                            
028200     SKIP2                                                                
028300     PERFORM UNTIL                                                        
028400     I41-IDUSER   NOT = I31-IDUSER   OR                                   
028500     I41-DAREGDAT NOT = I31-DAREGDAT OR                                   
028600     I41-TIREGTID NOT = I31-TIREGTID OR                                   
028700     I41-EOF = JA                                                         
028800        MOVE I41-IDUSER               TO WS-SORT-IDUSER                   
028900        MOVE I41-DAREGDAT             TO WS-SORT-DAREGDAT                 
029000        MOVE I41-TIREGTID             TO WS-SORT-TIREGTID                 
029200        MOVE I41-IDFKNGRP             TO WS-SORT-FKNGRP                   
029300        MOVE I41-IDDISTR              TO WS-SORT-BEGREPP                  
029400        MOVE I41-AREA                 TO SORT41-AREA                      
029410        IF ADDERA-PS = JA                                                 
029420            MOVE ZERO                     TO WS-SORT-PRODSL               
029430            MOVE ZERO                     TO SORT41-KDPRODSL              
029440            MOVE SPACE                    TO SORT41-BEPRODSL              
029450        ELSE                                                              
029460            MOVE I41-KDPRODSL             TO WS-SORT-PRODSL               
029470        END-IF                                                            
029500        RELEASE SORTERAD-POST41 FROM WS-SORTERAD-AREA                     
029600        PERFORM S02-LAS-41-FIL                                            
029700     END-PERFORM                                                          
029800     .                                                                    
029900     EJECT                                                                
030000 BAB-SORTERA-PA-KONCNR      SECTION.                                      
030010     MOVE 'BAB-SORTERA-PA-KONCNR' TO WS-SEKTION                           
030100     SKIP2                                                                
030200     PERFORM UNTIL                                                        
030300     I41-IDUSER   NOT = I31-IDUSER   OR                                   
030400     I41-DAREGDAT NOT = I31-DAREGDAT OR                                   
030500     I41-TIREGTID NOT = I31-TIREGTID OR                                   
030600     I41-EOF = JA                                                         
030700        MOVE I41-IDUSER               TO WS-SORT-IDUSER                   
030800        MOVE I41-DAREGDAT             TO WS-SORT-DAREGDAT                 
030900        MOVE I41-TIREGTID             TO WS-SORT-TIREGTID                 
031100        MOVE I41-IDFKNGRP             TO WS-SORT-FKNGRP                   
031200        MOVE I41-IDKONCNR             TO WS-SORT-BEGREPP                  
031300        MOVE I41-AREA                 TO SORT41-AREA                      
031310        IF ADDERA-PS = JA                                                 
031320            MOVE ZERO                     TO WS-SORT-PRODSL               
031330            MOVE ZERO                     TO SORT41-KDPRODSL              
031340            MOVE SPACE                    TO SORT41-BEPRODSL              
031350        ELSE                                                              
031360            MOVE I41-KDPRODSL             TO WS-SORT-PRODSL               
031370        END-IF                                                            
031400        RELEASE SORTERAD-POST41 FROM WS-SORTERAD-AREA                     
031500        PERFORM S02-LAS-41-FIL                                            
031600     END-PERFORM                                                          
031700     .                                                                    
031800     EJECT                                                                
031900 BAC-SORTERA-PA-MARKN       SECTION.                                      
031910     MOVE 'BAC-SORTERA-PA-MARKN' TO WS-SEKTION                            
032000     SKIP2                                                                
032100     PERFORM UNTIL                                                        
032200     I41-IDUSER   NOT = I31-IDUSER   OR                                   
032300     I41-DAREGDAT NOT = I31-DAREGDAT OR                                   
032400     I41-TIREGTID NOT = I31-TIREGTID OR                                   
032500     I41-EOF = JA                                                         
032600        MOVE I41-IDUSER               TO WS-SORT-IDUSER                   
032700        MOVE I41-DAREGDAT             TO WS-SORT-DAREGDAT                 
032800        MOVE I41-TIREGTID             TO WS-SORT-TIREGTID                 
033000        MOVE I41-IDFKNGRP             TO WS-SORT-FKNGRP                   
033100        MOVE I41-KDMARK-BUDG          TO WS-SORT-BEGREPP                  
033200        MOVE I41-AREA                 TO SORT41-AREA                      
033210        IF ADDERA-PS = JA                                                 
033220            MOVE ZERO                     TO WS-SORT-PRODSL               
033230            MOVE ZERO                     TO SORT41-KDPRODSL              
033240            MOVE SPACE                    TO SORT41-BEPRODSL              
033250        ELSE                                                              
033260            MOVE I41-KDPRODSL             TO WS-SORT-PRODSL               
033270        END-IF                                                            
033300        RELEASE SORTERAD-POST41 FROM WS-SORTERAD-AREA                     
033400        PERFORM S02-LAS-41-FIL                                            
033500     END-PERFORM                                                          
033600     .                                                                    
033700     EJECT                                                                
033800 BAD-SORTERA-PA-WORLD-WIDE  SECTION.                                      
033810     MOVE 'BAC-SORTERA-PA-WORLD-WIDE' TO WS-SEKTION                       
033900     SKIP2                                                                
034000     PERFORM UNTIL                                                        
034100     I41-IDUSER   NOT = I31-IDUSER   OR                                   
034200     I41-DAREGDAT NOT = I31-DAREGDAT OR                                   
034300     I41-TIREGTID NOT = I31-TIREGTID OR                                   
034400     I41-EOF = JA                                                         
034500        MOVE I41-IDUSER               TO WS-SORT-IDUSER                   
034600        MOVE I41-DAREGDAT             TO WS-SORT-DAREGDAT                 
034700        MOVE I41-TIREGTID             TO WS-SORT-TIREGTID                 
034900        MOVE I41-IDFKNGRP             TO WS-SORT-FKNGRP                   
035000        MOVE I41-KDMARK-BUDG          TO WS-SORT-BEGREPP                  
035100        MOVE I41-AREA                 TO SORT41-AREA                      
035110        IF ADDERA-PS = JA                                                 
035120            MOVE ZERO                     TO WS-SORT-PRODSL               
035130            MOVE ZERO                     TO SORT41-KDPRODSL              
035131            MOVE SPACE                    TO SORT41-BEPRODSL              
035140        ELSE                                                              
035150            MOVE I41-KDPRODSL             TO WS-SORT-PRODSL               
035160        END-IF                                                            
035200        RELEASE SORTERAD-POST41 FROM WS-SORTERAD-AREA                     
035300        PERFORM S02-LAS-41-FIL                                            
035400     END-PERFORM                                                          
035500     .                                                                    
035600     EJECT                                                                
035700 BB-RELEASE-P1-POST       SECTION.                                        
035710     MOVE 'BB-RELEASE-P1-POST' TO WS-SEKTION                              
035800     SKIP2                                                                
035900     MOVE I31-IDUSER                  TO WS-SORT-IDUSER                   
036000     MOVE I31-DAREGDAT                TO WS-SORT-DAREGDAT                 
036100     MOVE I31-TIREGTID                TO WS-SORT-TIREGTID                 
036200     MOVE ZERO                        TO WS-SORT-PRODSL                   
036300                                         WS-SORT-FKNGRP                   
036400                                         WS-SORT-BEGREPP                  
036500     IF I31-IDTRANS = '3202'                                              
036600        MOVE I31-AREA                 TO SORT231-AREA                     
036700        RELEASE SORTERAD-POST231 FROM WS-SORTERAD-AREA                    
036800     ELSE                                                                 
036900        MOVE I31-AREA                 TO SORT331-AREA                     
037000        RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                       
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 C-BEHANDLA-OCH-SKRIV     SECTION.                                        
037410     MOVE 'C-BEHANDLA-OCH-SKRIV' TO WS-SEKTION                            
037500     SKIP2                                                                
037600     PERFORM S03-RETURN                                                   
037700     PERFORM UNTIL SORT-EOF = JA                                          
037800        IF WS-SORT-IDGTYP < +5                                            
037900           IF WS-SORT-IDTRANS = '3202'                                    
038000              MOVE SORT231-KDNIVA           TO WS-KDNIVA                  
038100           ELSE                                                           
038200              MOVE 4                        TO WS-KDNIVA                  
038300           END-IF                                                         
038400           PERFORM S03-RETURN                                             
038500        ELSE                                                              
038600           PERFORM S06-INITIERA-U43-SPAR                                  
038700           MOVE WS-SORT-PRODSL              TO SPAR-PRODSL                
038800           MOVE WS-SORT-FKNGRP              TO SPAR-IDFKNGRP              
038900                                               WS-IDFKNGRP                
039000           MOVE WS-SORT-BEGREPP             TO SPAR-BEGREPP               
039100           PERFORM UNTIL WS-SORT-IDGTYP < 5 OR                            
039200           SORT-EOF = JA                                                  
039300              PERFORM CA-TESTA-NIVA-SUMMERA                               
039400              PERFORM CB-ADDERA                                           
039500              PERFORM S03-RETURN                                          
039600              MOVE WS-SORT-FKNGRP         TO WS-IDFKNGRP                  
039700           END-PERFORM                                                    
039800           PERFORM S05-SKRIV-BEGREPP                                      
039900        END-IF                                                            
040000     END-PERFORM                                                          
040100     PERFORM S05-SKRIV-BEGREPP                                            
040200     .                                                                    
040300     EJECT                                                                
040400 CA-TESTA-NIVA-SUMMERA   SECTION.                                         
040410     MOVE 'CA-TESTA-NIVA-SUMMERA' TO WS-SEKTION                           
040500     SKIP2                                                                
040600     IF SPAR-PRODSL = WS-SORT-PRODSL                                      
040700        IF SPAR-IDFKNGRP = WS-IDFKNGRP                                    
040800           IF SPAR-BEGREPP  = WS-SORT-BEGREPP                             
040900              CONTINUE                                                    
041000           ELSE                                                           
041100              PERFORM S05-SKRIV-BEGREPP                                   
041200              MOVE WS-SORT-BEGREPP TO SPAR-BEGREPP                        
041300           END-IF                                                         
041400        ELSE                                                              
041500           PERFORM S05-SKRIV-BEGREPP                                      
041600           MOVE WS-SORT-BEGREPP    TO SPAR-BEGREPP                        
041700           MOVE WS-IDFKNGRP        TO SPAR-IDFKNGRP                       
041800        END-IF                                                            
041900     ELSE                                                                 
042000        PERFORM S05-SKRIV-BEGREPP                                         
042100        MOVE WS-SORT-BEGREPP       TO SPAR-BEGREPP                        
042200        MOVE WS-IDFKNGRP           TO SPAR-IDFKNGRP                       
042300        MOVE WS-SORT-PRODSL        TO SPAR-PRODSL                         
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700 CB-ADDERA                  SECTION.                                      
042710     MOVE 'CB-ADDERA' TO WS-SEKTION                                       
042800     SKIP2                                                                
042900     ADD  SORT41-SUARTFSG-PER    TO U43-SUARTFSG-PER                      
043000     ADD  SORT41-SUARTFSG-AAR    TO U43-SUARTFSG-AAR                      
043100     ADD  SORT41-SUARTFSG-FAAR   TO U43-SUARTFSG-FAAR                     
043200     ADD  SORT41-SUARTFSG-RAAR   TO U43-SUARTFSG-RAAR                     
043300     ADD  SORT41-SUARTFSG-FRAAR  TO U43-SUARTFSG-FRAAR                    
043400     ADD  SORT41-SULEVANT-PER    TO U43-SULEVANT-PER                      
043500     ADD  SORT41-SULEVANT-AAR    TO U43-SULEVANT-AAR                      
043600     ADD  SORT41-SULEVANT-FAAR   TO U43-SULEVANT-FAAR                     
043700     ADD  SORT41-SULEVANT-RAAR   TO U43-SULEVANT-RAAR                     
043800     ADD  SORT41-SULEVANT-FRAAR  TO U43-SULEVANT-FRAAR                    
043900     ADD  SORT41-SUARTSJK-PER    TO U43-SUARTSJK-PER                      
044000     ADD  SORT41-SUARTSJK-AAR    TO U43-SUARTSJK-AAR                      
044100     ADD  SORT41-SUARTSJK-FAAR   TO U43-SUARTSJK-FAAR                     
044200     ADD  SORT41-SUARTSJK-RAAR   TO U43-SUARTSJK-RAAR                     
044300     ADD  SORT41-SUARTSJK-FRAAR  TO U43-SUARTSJK-FRAAR                    
044400     .                                                                    
044500     EJECT                                                                
044600 S01-LAS-31-FIL    SECTION.                                               
044610     MOVE 'S01-LAS-31-FIL'        TO WS-FIL-SEKTION                       
044700     SKIP2                                                                
044800     READ W33031S INTO I31-AREA                                           
044900     AT END                                                               
045000       MOVE JA                    TO I31-EOF                              
045100     END-READ                                                             
045200                                                                          
045300     IF I31-EOF = NEJ                                                     
045400       MOVE '    '                TO POSTSUM-TRANSTYP                     
045500       MOVE 'W33031'              TO POSTSUM-FDNAMN                       
045600       MOVE 'W33042D1'            TO POSTSUM-DDNAMN2                      
045700       CALL POSTSUM USING POSTSUM-PARM                                    
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 S02-LAS-41-FIL   SECTION.                                                
046110     MOVE 'S02-LAS-41-FIL'        TO WS-FIL-SEKTION                       
046200     SKIP2                                                                
046300     READ W33041S INTO I41-AREA                                           
046400     AT END                                                               
046500       MOVE JA                    TO I41-EOF                              
046600       MOVE +999999               TO I41-DAREGDAT                         
046700                                     I41-TIREGTID                         
046800     END-READ                                                             
046900                                                                          
047000     IF I41-EOF = NEJ                                                     
047100       MOVE '    '                TO POSTSUM-TRANSTYP                     
047200       MOVE 'W33041'              TO POSTSUM-FDNAMN                       
047300       MOVE 'W33042D2'            TO POSTSUM-DDNAMN2                      
047400       CALL POSTSUM USING POSTSUM-PARM                                    
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 S03-RETURN       SECTION.                                                
047810     MOVE 'S03-RETURN'        TO WS-FIL-SEKTION                           
047900     SKIP2                                                                
048000     RETURN SORTFIL INTO WS-SORTERAD-AREA                                 
048100     AT END                                                               
048200       MOVE JA                    TO SORT-EOF                             
048300     END-RETURN                                                           
048400                                                                          
048500     IF SORT-EOF = NEJ                                                    
048600       MOVE 'SORT'                TO POSTSUM-TRANSTYP                     
048700       MOVE '      '              TO POSTSUM-FDNAMN                       
048800       MOVE 'W33042DS'            TO POSTSUM-DDNAMN2                      
048900       CALL POSTSUM USING POSTSUM-PARM                                    
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 S04-SKRIV-43-FIL         SECTION.                                        
049310     MOVE 'S04-SKRIV-43-FIL'  TO WS-FIL-SEKTION                           
049400     SKIP2                                                                
049500     WRITE U43-POST FROM U43-AREA                                         
049600     MOVE '    '                 TO POSTSUM-TRANSTYP                      
049700     MOVE 'W33043'               TO POSTSUM-FDNAMN                        
049800     MOVE 'W33042D3'             TO POSTSUM-DDNAMN2                       
049900     CALL POSTSUM USING POSTSUM-PARM                                      
050000     .                                                                    
050100     EJECT                                                                
050200 S05-SKRIV-BEGREPP             SECTION.                                   
050210     MOVE 'S05-SKRIV-BEGREPP' TO WS-FIL-SEKTION                           
050300     SKIP2                                                                
050400     IF U43-IDGTYP = +5                                                   
050500        MOVE 99999                      TO U43-IDFKNGRP                   
050600        PERFORM S04-SKRIV-43-FIL                                          
050700        IF WS-KDNIVA = ZERO                                               
050800           CONTINUE                                                       
050900        ELSE                                                              
051000           IF WS-KDNIVA = 4                                               
051100              MOVE SPAR-IDFKNGRP        TO U43-IDFKNGRP                   
051200              PERFORM S04-SKRIV-43-FIL                                    
051300              IF SPAR-FKNGRP-POS-4 NOT = ZERO                             
051400                 COMPUTE U43-IDFKNGRP = 10 * SPAR-4-FOERSTA               
051500                 MOVE SPACE             TO U43-BEFKNGRP                   
051600                 PERFORM S04-SKRIV-43-FIL                                 
051700              END-IF                                                      
051800           END-IF                                                         
051900           IF SPAR-FKNGRP-POS-3 NOT = ZERO                                
052000              COMPUTE U43-IDFKNGRP = 100 * SPAR-3-FOERSTA                 
052100              PERFORM S04-SKRIV-43-FIL                                    
052200           END-IF                                                         
052300        END-IF                                                            
052400        COMPUTE U43-IDFKNGRP = 1000 * SPAR-2-FOERSTA                      
052500        PERFORM S04-SKRIV-43-FIL                                          
052600        PERFORM S06-INITIERA-U43-SPAR                                     
052700     END-IF                                                               
052800     .                                                                    
052900    EJECT                                                                 
053000 S06-INITIERA-U43-SPAR      SECTION.                                      
053010     MOVE 'S06-INITIERA-U43' TO WS-FIL-SEKTION                            
053100     SKIP2                                                                
053200     MOVE 43NTLZD-AREA                         TO U43-AREA                
053300     IF SORT-EOF = NEJ                                                    
053400        IF WS-SORT-IDGTYP = +5                                            
053500           MOVE SORT41-001-GRUPP               TO U43-001-GRUPP           
053600           MOVE SORT41-002-GRUPP               TO U43-002-GRUPP           
053700        END-IF                                                            
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 Z-FINIT SECTION.                                                         
054110     MOVE 'Z-FINIT'          TO WS-SEKTION                                
054200     SKIP2                                                                
054300     CLOSE W33031S                                                        
054400           W33041S                                                        
054500           W33043                                                         
054600     MOVE 'S' TO POSTSUM-OPKOD                                            
054700     CALL POSTSUM USING POSTSUM-PARM                                      
054800     .                                                                    
054900     EJECT                                                                
