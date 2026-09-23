000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W3307400.                                    
000300 AUTHOR.                     STEFAN KIHLBERG                              
000400 DATE-WRITTEN.               JAN-90.                                      
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700                                                                          
000800*    FUNKTION.                                                            
000900                                                                          
001000*    ARTIKELSTATISTIK.                                                    
001100*    SUMMERAR RESULTAT AV URVAL FRÅN BILD 3202 3203                       
001200*    URVAL AV TVÅ TYPER KOMMER FRÅN FILEN W33035, DENNA STANDARD-         
001300*    SORTERAS (STIGANDE) PÅ URVAL I PROCEDUREN. RESULTAT AV URVAL         
001400*    KOMMER FRÅN FILEN W33073.  URVALEN SUMMERAS PÅ ARTIKELNUMMER/        
001500*    NIVÅTYP OCH SKRIVS I SUMMAPOSTER EFTER RESP URVAL PÅ FILEN           
001600*    W33075.                                                              
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*    --- INFILER:                                                         
002700     SELECT SORTFIL                      ASSIGN TO W33074DS.              
002800     SELECT W33035S                      ASSIGN TO W33074D1.              
002900     SELECT W33073                       ASSIGN TO W33074D2.              
003000*    --- UTFILER:                                                         
003100     SELECT W33075                       ASSIGN TO W33074D3.              
003200     EJECT                                                                
003300                                                                          
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800                                                                          
003900 SD  SORTFIL                                                              
004000     RECORDING V                                                          
004100     SKIP2                                                                
004200 01  SORTERAD-POST.                                                       
004300   03  SORT-IDUSER                 PIC X(8).                              
004400   03  SORT-DAREGDAT               PIC X(8).                              
004500   03  SORT-TIREGTID               PIC X(7).                              
004600   03  SORT-FOERSTA                PIC X(9).                              
004700   03  SORT-ANDRA                  PIC X(9).                              
004800   03  SORT-TREDJE                 PIC X(5).                              
004900   03  CTEXT.                                                             
005000     05  FILLER                    PIC X(154).                            
005100                                                                          
005200*03  POST -COPY W33073  -L -PRE SRT73- -RED CTEXT.                        
005300*++INCLUDE W33073                                                         
005400     EJECT                                                                
005500                                                                          
005600 FD  W33035S                                                              
005700     LABEL RECORD   STANDARD                                              
005800     RECORDING      V                                                     
005900     BLOCK CONTAINS 0.                                                    
006000     SKIP2                                                                
006100                                                                          
006200*01  POST -COPY W3303503  -L -PRE I335-                                   
006300*++INCLUDE W3303503                                                       
006400     EJECT                                                                
006500                                                                          
006600*01  POST -COPY W3303502  -L -PRE I235-                                   
006700*++INCLUDE W3303502                                                       
006800     EJECT                                                                
006900                                                                          
007000 FD  W33073                                                               
007100     LABEL RECORD   STANDARD                                              
007200     RECORDING      F                                                     
007300     BLOCK CONTAINS 0.                                                    
007400     SKIP2                                                                
007500*01  POST -COPY W33073  -L -PRE I73-.                                     
007600*++INCLUDE W33073                                                         
007700     EJECT                                                                
007800                                                                          
007900 FD  W33075                                                               
008000     LABEL RECORD   STANDARD                                              
008100     RECORDING      V                                                     
008200     BLOCK CONTAINS 0.                                                    
008300     SKIP2                                                                
008400*01  POST -COPY W3303503  -L -PRE U375-.                                  
008500*++INCLUDE W3303503                                                       
008600     SKIP2                                                                
008700                                                                          
008800*01  POST -COPY W3303502  -L -PRE U275-.                                  
008900*++INCLUDE W3303502                                                       
009000     EJECT                                                                
009100                                                                          
009200*01  POST -COPY W33075  -L -PRE U75-.                                     
009300*++INCLUDE W33075                                                         
009400     SKIP2                                                                
009500                                                                          
009600 WORKING-STORAGE SECTION.                                                 
009610                                                                          
009620*   --  CHECKED BY WY2000                                                 
009700     SKIP2                                                                
009800                                                                          
009900 01  FILLER                       PIC X(16)  VALUE 'W-S SECTION'.         
010000                                                                          
010100 77  PROGRAM-NAMN            PIC X(8)       VALUE 'W3307400'.             
010200 77  JA                      PIC X          VALUE 'J'.                    
010300 77  NEJ                     PIC X          VALUE 'N'.                    
010400 77  FELKOD                  PIC S9(4)      VALUE +16 COMP SYNC.          
010500 77  WS-BRYT-VECKA           PIC  9(6)      VALUE ZERO .                  
010600 77  WS-BRYT-INP-IDARTNR     PIC S9(9)      VALUE ZERO COMP-3.            
010700 77  WS-BRYT-OUTP-IDARTNR    PIC S9(9)      VALUE ZERO COMP-3.            
010800                                                                          
010900 77  WS-SULEVANT-VV          PIC S9(9)      VALUE ZERO COMP-3.            
011000 77  WS-SUARTFSG-VV          PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011100 77  WS-RETOTBV-VV           PIC S9(9)V9(2) VALUE ZERO COMP-3.            
011200 77  WS-SUARTSJK-VV          PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011300 77  WS-SULEVANT-FVV         PIC S9(9)      VALUE ZERO COMP-3.            
011400 77  WS-SUARTFSG-FVV         PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011500 77  WS-RETOTBV-FVV          PIC S9(9)V9(2) VALUE ZERO COMP-3.            
011600 77  WS-SUARTSJK-FVV         PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011700 77  W-SUARTSJK-VV           PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011800 77  W-SUARTSJK-FVV          PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011900                                                                          
012000 01  FILLER                  PIC X(16)     VALUE 'SWITCHAR'.              
012100                                                                          
012200 01    SWITCHAR.                                                          
012300                                                                          
012400    03  W33035-EOF           PIC X(1)       VALUE 'N'.                    
012500    03  W33073-EOF           PIC X(1)       VALUE 'N'.                    
012600    03  SORT-EOF             PIC X(1)       VALUE 'N'.                    
012700                                                                          
012800                                                                          
012900                                                                          
013000 01  FILLER                       PIC X(16)  VALUE 'DYN SUBPGM '.         
013100                                                                          
013200 01  DYNAMISKA-SUBPROGRAM.                                                
013300*                                                                         
013400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013600     SKIP2                                                                
013700*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
013800*                                                                         
013900 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
014000*01  -COPY W0005 -PRE  POSTSUM-                                           
014100*++INCLUDE W0005                                                          
014200     EJECT                                                                
014300*                                                                         
014400 01  FILLER                       PIC X(16)  VALUE 'WS-SORT-A'.           
014500                                                                          
014600 01  WS-SORTERAD-AREA.                                                    
014700   03  WS-SORT-IDUSER              PIC X(8).                              
014800   03  WS-SORT-DAREGDAT            PIC X(8).                              
014900   03  WS-SORT-TIREGTID            PIC X(7).                              
015000   03  WS-SORT-FOERSTA             PIC X(9).                              
015100   03  WS-SORT-ANDRA               PIC X(9).                              
015200   03  WS-SORT-TREDJE              PIC X(5).                              
015300   03  WS-CTEXT.                                                          
015400     05  FILLER                      PIC X(154).                          
015500                                                                          
015600*03  AREA -COPY W33073  -PRE SORTWS- -RED WS-CTEXT.                       
015700*++INCLUDE W33073                                                         
015800     EJECT                                                                
015900                                                                          
016000                                                                          
016100 01  FILLER                       PIC X(16)  VALUE 'WS75-AREA  '.         
016200                                                                          
016300*01  AREA -COPY W33075  -PRE WS75-.                                       
016400*++INCLUDE W33075                                                         
016500     EJECT                                                                
016600 01  FILLER                       PIC X(16)  VALUE 'WI 35-FILEN'.         
016700                                                                          
016800 01  WI35-AREA                   PIC X(2625).                             
016900                                                                          
017000*01  AREA  -COPY W3303503  -PRE WIG35- -RED WI35-AREA.                    
017100*++INCLUDE W3303503                                                       
017200    EJECT                                                                 
017300                                                                          
017400*01  AREA  -COPY W3303503  -PRE WI335- -RED WI35-AREA.                    
017500*++INCLUDE W3303503                                                       
017600    EJECT                                                                 
017700                                                                          
017800*01  AREA  -COPY W3303502  -PRE WI235- -RED WI35-AREA.                    
017900*++INCLUDE W3303502                                                       
018000    EJECT                                                                 
018100                                                                          
018200 01  FILLER                       PIC X(16)  VALUE 'I73-FILEN'.           
018300                                                                          
018400                                                                          
018500*01  AREA  -COPY W33073  -PRE WI73-.                                      
018600*++INCLUDE W33073                                                         
018700    EJECT                                                                 
018800                                                                          
018900                                                                          
019000                                                                          
019100 01  FILLER                       PIC X(16)  VALUE 'UTFIL'.               
019200                                                                          
019300 01  WU75-AREA                   PIC X(2625).                             
019400                                                                          
019500*01  AREA  -COPY W3303503  -PRE WU375-  REDIFINES WU75-AREA.              
019600*++INCLUDE W3303503                                                       
019700    EJECT                                                                 
019800                                                                          
019900*01  AREA  -COPY W3303502  -PRE WU275- REDIFINES WU75-AREA.               
020000*++INCLUDE W3303502                                                       
020100    EJECT                                                                 
020200                                                                          
020300*01  AREA  -COPY W33075  -PRE WU075- REDIFINES WU75-AREA.                 
020400*++INCLUDE W33075                                                         
020500     EJECT                                                                
020600                                                                          
020700 PROCEDURE DIVISION.                                                      
020800    SKIP2                                                                 
020900 STYR SECTION.                                                            
021000     PERFORM A-INIT                                                       
021100     PERFORM S01-LAS-73-FIL                                               
021200     PERFORM S02-LAS-35-FIL                                               
021300     PERFORM UNTIL W33035-EOF = JA                                        
021400        IF WI73-IDUSER   = WIG35-IDUSER   AND                             
021500           WI73-DAREGDAT = WIG35-DAREGDAT AND                             
021600           WI73-TIREGTID = WIG35-TIREGTID                                 
021700           MOVE WIG35-DAFSGVV-FOM   TO WS-BRYT-VECKA                      
021800           PERFORM B1-SKRIVA-MATCHAT-URVAL                                
021900           SORT SORTFIL                                                   
022000              ASCENDING KEY SORT-IDUSER                                   
022100                            SORT-DAREGDAT                                 
022200                            SORT-TIREGTID                                 
022300                            SORT-FOERSTA                                  
022400                            SORT-ANDRA                                    
022500                            SORT-TREDJE                                   
022600              INPUT PROCEDURE C-SORTERA-RESULTAT-PER-URVAL                
022700              OUTPUT PROCEDURE D-SUMMERA-SKRIVA-RESULTAT                  
022800              MOVE NEJ TO SORT-EOF                                        
022900           IF SORT-RETURN = ZERO                                          
023000              CONTINUE                                                    
023100           ELSE                                                           
023200              DISPLAY 'FEL I SORTERINGEN'                                 
023300              CALL ABEND USING FELKOD                                     
023400           END-IF                                                         
023500          PERFORM S02-LAS-35-FIL                                          
023600        ELSE                                                              
023700          PERFORM B2-SKRIVA-EJ-MATCHAT-URVAL                              
023800          PERFORM S02-LAS-35-FIL                                          
023900        END-IF                                                            
024000     END-PERFORM                                                          
024100     PERFORM Z-FINIT                                                      
024200     MOVE ZERO TO RETURN-CODE                                             
024300     GOBACK                                                               
024400     .                                                                    
024500     EJECT                                                                
024600 A-INIT SECTION.                                                          
024700     SKIP2                                                                
024800     OPEN INPUT  W33035S                                                  
024900                 W33073                                                   
025000     OPEN OUTPUT W33075                                                   
025100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
025200     .                                                                    
025300     EJECT                                                                
025400                                                                          
025500                                                                          
025600                                                                          
025700                                                                          
025800 B1-SKRIVA-MATCHAT-URVAL SECTION.                                         
025900                                                                          
026000***  SKRIVA URVALSPOSTER FÖR LISTA VA1 FRÅN BILD 3202/3203                
026100                                                                          
026200     IF WIG35-IDPTYP = 'VA1'                                              
026300        EVALUATE  WIG35-IDTRANS                                           
026400           WHEN 3203                                                      
026500              MOVE WI335-AREA  TO WU375-AREA                              
026600              PERFORM S05-SKRIV-U375                                      
026700           WHEN 3202                                                      
026800              MOVE WI235-AREA  TO WU275-AREA                              
026900              PERFORM S04-SKRIV-U275                                      
027000        END-EVALUATE                                                      
027100     END-IF                                                               
027200     .                                                                    
027300                                                                          
027400                                                                          
027500 B2-SKRIVA-EJ-MATCHAT-URVAL SECTION.                                      
027600                                                                          
027700***  SKRIVA URVALSPOSTER FÖR LISTA VA1 FRÅN BILD 3202/3203                
027800                                                                          
027900     IF WIG35-IDPTYP = 'VA1'                                              
028000        EVALUATE  WIG35-IDTRANS                                           
028100           WHEN 3203                                                      
028200              MOVE WI335-AREA  TO WU375-AREA                              
028300              MOVE ZERO        TO WU375-IDGTYP                            
028400              PERFORM S05-SKRIV-U375                                      
028500           WHEN 3202                                                      
028600              MOVE WI235-AREA  TO WU275-AREA                              
028700              MOVE ZERO        TO WU275-IDGTYP                            
028800              PERFORM S04-SKRIV-U275                                      
028900        END-EVALUATE                                                      
029000     END-IF                                                               
029100     .                                                                    
029200 C-SORTERA-RESULTAT-PER-URVAL SECTION.                                    
029300                                                                          
029400                                                                          
029500***  LÄSER RESULTATPOSTER FÖR AKTUELLT URVAL OCH SKICKAR DESSA            
029600***  TILL COBOL-SORTETING.                                                
029700                                                                          
029800     PERFORM UNTIL                                                        
029900     (W33073-EOF = JA) OR                                                 
030000     (WI73-IDUSER  NOT = WIG35-IDUSER) OR                                 
030100     (WI73-DAREGDAT NOT = WIG35-DAREGDAT) OR                              
030200     (WI73-TIREGTID NOT = WIG35-TIREGTID)                                 
030300                                                                          
030400        MOVE WI73-IDUSER       TO WS-SORT-IDUSER                          
030500        MOVE WI73-DAREGDAT     TO WS-SORT-DAREGDAT                        
030600        MOVE WI73-TIREGTID     TO WS-SORT-TIREGTID                        
030700        MOVE WI73-IDARTNR      TO WS-SORT-FOERSTA                         
030800        MOVE WI73-DAFSGVV      TO WS-SORT-ANDRA                           
030900        MOVE WI73-AREA         TO SORTWS-AREA                             
031000        RELEASE SORTERAD-POST  FROM WS-SORTERAD-AREA                      
031100        PERFORM S01-LAS-73-FIL                                            
031200     END-PERFORM                                                          
031300     .                                                                    
031400                                                                          
031500 D-SUMMERA-SKRIVA-RESULTAT SECTION.                                       
031600                                                                          
031700*** HÄMTAR POSTER MED AKTUELLT URVAL FRÅN COBOL-SORTERING,                
031800*** SAMLAR UPPGIFTER PER ARTIKELNUMMER OCH GÖR BERÄKNINGAR PÅ             
031900*** DESSA.                                                                
032000                                                                          
032100     PERFORM S03-SORT-RETURN                                              
032200     PERFORM DA-NOLLSTALL-SUMFAELT                                        
032300     MOVE SORTWS-001-GRUPP     TO WS75-001-GRUPP                          
032400     MOVE SORTWS-IDARTNR       TO WS-BRYT-OUTP-IDARTNR                    
032500     PERFORM UNTIL SORT-EOF = JA                                          
032600        PERFORM UNTIL SORTWS-IDARTNR NOT = WS-BRYT-OUTP-IDARTNR           
032700        OR SORT-EOF = JA                                                  
032800           PERFORM DB-FLYTTA-RESULTATNAMN-WS75                            
032900           IF SORTWS-DAFSGVV < WS-BRYT-VECKA                              
033000              PERFORM DC-SAMLA-FVV                                        
033100           ELSE                                                           
033200              PERFORM DD-SAMLA-VV                                         
033300           END-IF                                                         
033400           PERFORM S03-SORT-RETURN                                        
033500        END-PERFORM                                                       
033600        IF WS-SUARTFSG-FVV NOT = ALL ZERO                                 
033700           PERFORM DE-SJK-TG-FVV                                          
033800        ELSE                                                              
033900           MOVE ALL ZERO TO WS-RETOTBV-FVV                                
034000        END-IF                                                            
034100        IF WS-SUARTFSG-VV NOT = ALL ZERO                                  
034200           PERFORM DF-SJK-TG-VV                                           
034300        ELSE                                                              
034400           MOVE ALL ZERO TO WS-RETOTBV-VV                                 
034500        END-IF                                                            
034600        PERFORM DG-FLYTTA-TILL-UTSKRIFT-WU075                             
034700        PERFORM S06-SKRIV-U075                                            
034800        PERFORM DA-NOLLSTALL-SUMFAELT                                     
034900        MOVE SORTWS-IDARTNR    TO WS-BRYT-OUTP-IDARTNR                    
035000     END-PERFORM                                                          
035100           .                                                              
035200                                                                          
035300 DA-NOLLSTALL-SUMFAELT SECTION.                                           
035400                                                                          
035500     INITIALIZE WS-SUARTSJK-FVV WS-SULEVANT-FVV                           
035600     INITIALIZE WS-SUARTFSG-FVV WS-RETOTBV-FVV                            
035700     INITIALIZE WS-SUARTSJK-VV WS-SULEVANT-VV                             
035800     INITIALIZE WS-SUARTFSG-VV WS-RETOTBV-VV.                             
035900                                                                          
036000                                                                          
036100 DB-FLYTTA-RESULTATNAMN-WS75 SECTION.                                     
036200                                                                          
036300     MOVE SORTWS-IDARTNR       TO WS75-IDARTNR                            
036400     MOVE SORTWS-BEART-SVE     TO WS75-BEART-SVE                          
036500     MOVE SORTWS-KDPRODSL      TO WS75-KDPRODSL                           
036600     MOVE SORTWS-BEPRODSL      TO WS75-BEPRODSL                           
036700     MOVE SORTWS-IDFKNGRP      TO WS75-IDFKNGRP                           
036800     MOVE SORTWS-BEFKNGRP      TO WS75-BEFKNGRP                           
036900     .                                                                    
037000                                                                          
037100                                                                          
037200  DC-SAMLA-FVV SECTION.                                                   
037300                                                                          
037400***  BERÄKNAR SJÄLVKOSTNAD PER ARTIKEL OCH VECKA,                         
037500***  SAMLAR UPPGIFTER PER ARTIKEL OM SJÄLVKOSTNAD, FÖRSÄLJNINGS-          
037600***  VÄRDE OCH FÖRSÅLT ANTAL (FÖREGÅENDE RULLANDE ÅR).                    
037700                                                                          
037800     INITIALIZE W-SUARTSJK-FVV                                            
037900     COMPUTE W-SUARTSJK-FVV =                                             
038000     SORTWS-SULEVANT * SORTWS-PRARTSJK                                    
038100                                                                          
038200     COMPUTE WS-SUARTSJK-FVV =                                            
038300     WS-SUARTSJK-FVV + W-SUARTSJK-FVV                                     
038400     COMPUTE WS-SUARTFSG-FVV =                                            
038500     WS-SUARTFSG-FVV + SORTWS-SUARTFSG                                    
038600     COMPUTE WS-SULEVANT-FVV =                                            
038700     WS-SULEVANT-FVV + SORTWS-SULEVANT.                                   
038800                                                                          
038900                                                                          
039000  DD-SAMLA-VV SECTION.                                                    
039100                                                                          
039200***  BERÄKNAR SJÄLVKOSTNAD PER ARTIKEL OCH VECKA,                         
039300***  SAMLAR UPPGIFTER PER ARTIKEL OM SJÄLVKOSTNAD, FÖRSÄLJNINGS-          
039400***  VÄRDE OCH FÖRSÅLT ANTAL (SENASTE RULLANDE ÅR).                       
039500                                                                          
039600     INITIALIZE W-SUARTSJK-VV                                             
039700     COMPUTE W-SUARTSJK-VV =                                              
039800     SORTWS-SULEVANT * SORTWS-PRARTSJK                                    
039900                                                                          
040000     COMPUTE WS-SUARTSJK-VV =                                             
040100     WS-SUARTSJK-VV + W-SUARTSJK-VV                                       
040200     COMPUTE WS-SUARTFSG-VV =                                             
040300     WS-SUARTFSG-VV + SORTWS-SUARTFSG                                     
040400     COMPUTE WS-SULEVANT-VV =                                             
040500     WS-SULEVANT-VV + SORTWS-SULEVANT.                                    
040600                                                                          
040700                                                                          
040800  DE-SJK-TG-FVV SECTION.                                                  
040900                                                                          
041000***  BERÄKNAR TÄCKNINGSBIDRAG PER SÖKT ARTIKEL OCH SÖKTA VECKOR.          
041100***  OM TÄCKNINGSBIDRAG UNDERSTIGER -99.9% ELLER ÖVERSTIGER               
041200***  99.9% ANGES TÄCKNINGSBIDRAGET SOM -99.9% RESP 99.9%.                 
041300***  (FÖREGÅENDE RULLANDE ÅR)                                             
041400                                                                          
041500     COMPUTE WS-RETOTBV-FVV ROUNDED =                                     
041600     100 * (( WS-SUARTFSG-FVV -                                           
041700     WS-SUARTSJK-FVV ) / WS-SUARTFSG-FVV).                                
041800                                                                          
041900     IF WS-RETOTBV-FVV < -99.9                                            
042000        MOVE -99.9 TO WS-RETOTBV-FVV                                      
042100     ELSE                                                                 
042200        IF WS-RETOTBV-FVV > 99.9                                          
042300           MOVE 99.9 TO WS-RETOTBV-FVV                                    
042400        END-IF                                                            
042500     END-IF .                                                             
042600                                                                          
042700  DF-SJK-TG-VV SECTION.                                                   
042800                                                                          
042900***  BERÄKNAR TÄCKNINGSBIDRAG PER SÖKT ARTIKEL OCH SÖKTA VECKOR.          
043000***  OM TÄCKNINGSBIDRAG UNDERSTIGER -99.9% ELLER ÖVERSTIGER               
043100***  99.9% ANGES TÄCKNINGSBIDRAGET SOM -99.9% RESP 99.9%.                 
043200***  (SENASTE RULLANDE ÅR)                                                
043300                                                                          
043400     COMPUTE WS-RETOTBV-VV ROUNDED =                                      
043500     100 * (( WS-SUARTFSG-VV -                                            
043600     WS-SUARTSJK-VV )  / WS-SUARTFSG-VV).                                 
043700                                                                          
043800     IF WS-RETOTBV-FVV < -99.9                                            
043900        MOVE -99.90 TO WS-RETOTBV-FVV                                     
044000     ELSE                                                                 
044100        IF WS-RETOTBV-FVV > 99.9                                          
044200           MOVE 99.90 TO WS-RETOTBV-FVV                                   
044300        END-IF                                                            
044400     END-IF .                                                             
044500                                                                          
044600                                                                          
044700 DG-FLYTTA-TILL-UTSKRIFT-WU075 SECTION.                                   
044800                                                                          
044900     MOVE WS75-001-GRUPP       TO WU075-001-GRUPP                         
045000                                                                          
045100     MOVE WS75-IDARTNR         TO WU075-IDARTNR                           
045200     MOVE WS75-BEART-SVE       TO WU075-BEART-SVE                         
045300     MOVE WS75-KDPRODSL        TO WU075-KDPRODSL                          
045400     MOVE WS75-BEPRODSL        TO WU075-BEPRODSL                          
045500     MOVE WS75-IDFKNGRP        TO WU075-IDFKNGRP                          
045600     MOVE WS75-BEFKNGRP        TO WU075-BEFKNGRP                          
045700                                                                          
045800     MOVE WS-SULEVANT-FVV      TO WU075-SULEVANT-FVV                      
045900     MOVE WS-SUARTFSG-FVV      TO WU075-SUARTFSG-FVV                      
046000     MOVE WS-RETOTBV-FVV       TO WU075-RETOTBV-FVV                       
046100     MOVE WS-SUARTSJK-FVV      TO WU075-SUARTSJK-FVV                      
046200     MOVE WS-SULEVANT-VV       TO WU075-SULEVANT-VV                       
046300     MOVE WS-SUARTFSG-VV       TO WU075-SUARTFSG-VV                       
046400     MOVE WS-RETOTBV-VV        TO WU075-RETOTBV-VV                        
046500     MOVE WS-SUARTSJK-VV       TO WU075-SUARTSJK-VV                       
046600     .                                                                    
046700                                                                          
046800 S01-LAS-73-FIL   SECTION.                                                
046900     SKIP2                                                                
047000     READ W33073 INTO WI73-AREA                                           
047100     AT END                                                               
047200       MOVE JA                 TO W33073-EOF                              
047300     END-READ                                                             
047400                                                                          
047500     IF W33073-EOF = NEJ                                                  
047600       MOVE '    '             TO POSTSUM-TRANSTYP                        
047700       MOVE 'W33073'           TO POSTSUM-FDNAMN                          
047800       MOVE 'W33074D2'         TO POSTSUM-DDNAMN2                         
047900       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
048400 S02-LAS-35-FIL SECTION.                                                  
048500     SKIP2                                                                
048600     READ W33035S INTO WI35-AREA                                          
048700     AT END                                                               
048800       MOVE JA                 TO W33035-EOF                              
048900     END-READ                                                             
049000                                                                          
049100     IF W33035-EOF = NEJ                                                  
049200       MOVE '    '             TO POSTSUM-TRANSTYP                        
049300       MOVE 'W33035'           TO POSTSUM-FDNAMN                          
049400       MOVE 'W33074D1'         TO POSTSUM-DDNAMN2                         
049500       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
049600     END-IF                                                               
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000 S03-SORT-RETURN SECTION.                                                 
050100     SKIP2                                                                
050200     RETURN SORTFIL INTO WS-SORTERAD-AREA                                 
050300     AT END                                                               
050400       MOVE JA                TO SORT-EOF                                 
050500     END-RETURN                                                           
050600                                                                          
050700     IF SORT-EOF = NEJ                                                    
050800       MOVE 'SORT'            TO POSTSUM-TRANSTYP                         
050900       MOVE '      '          TO POSTSUM-FDNAMN                           
051000       MOVE '        '        TO POSTSUM-DDNAMN2                          
051100       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
051200     END-IF                                                               
051300     .                                                                    
051400     EJECT                                                                
051500                                                                          
051600 S04-SKRIV-U275 SECTION.                                                  
051700     SKIP1                                                                
051800     WRITE U275-POST           FROM WU275-AREA                            
051900     MOVE '275'                TO POSTSUM-TRANSTYP                        
052000     MOVE 'W33074D3'           TO POSTSUM-DDNAMN2                         
052100     MOVE 'W33075'             TO POSTSUM-FDNAMN                          
052200     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
052300     .                                                                    
052400 S05-SKRIV-U375 SECTION.                                                  
052500     SKIP1                                                                
052600     WRITE U375-POST           FROM WU375-AREA                            
052700     MOVE '375'                TO POSTSUM-TRANSTYP                        
052800     MOVE 'W33074D3'           TO POSTSUM-DDNAMN2                         
052900     MOVE 'W33075'             TO POSTSUM-FDNAMN                          
053000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
053100     .                                                                    
053200 S06-SKRIV-U075 SECTION.                                                  
053300     SKIP1                                                                
053400     WRITE U75-POST            FROM WU075-AREA                            
053500     MOVE '075'                TO POSTSUM-TRANSTYP                        
053600     MOVE 'W33074D3'           TO POSTSUM-DDNAMN2                         
053700     MOVE 'W33075'             TO POSTSUM-FDNAMN                          
053800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
053900     .                                                                    
054000 Z-FINIT SECTION.                                                         
054100     SKIP2                                                                
054200     CLOSE W33035S                                                        
054300           W33073                                                         
054400           W33075                                                         
054500     MOVE 'S' TO POSTSUM-OPKOD                                            
054600     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
054700     .                                                                    
