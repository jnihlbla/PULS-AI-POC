000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5403000.                                                
000300*AUTHOR.         KARL JOHAN HANSSON.                                      
000400*DATE-WRITTEN.   FEBRUARI 1996.                                           
000500*REMARKS.                                                                 
000600*    FUNKTION:  PROGRAMMET LÄSER ARTIKELFIL OCH VÄLJER                    
000700*               UT ARTIKLAR ENLIGT FÖLJANDE:                              
000800*             - ALLA ARTIKLAR MED NÅGOT KALKYLPÅLÄGG EJ NOLL              
000900*               MED KALKYLPÅLÄGG MENAS PRDIRLON PRDMTRL PROVRPAL          
001000*               FÖR DESSA BRÄKNAS KDVTH MED HJÄLP AV SUBMODUL             
001100*               W510VTH. OM BERÄKNADE KDVTH EJ STÄMMER MED ART-           
001200*               REGISTRETS KDVTH, SKPAS EN UPPDATERINGSTRANS MED          
001300*               DETTTA NYA FRAMRÄKNADE KDVTH.                             
001400*             - ALLA ÖVRIGA ARTIKLAR DÄR KDVTH EJ ÄR NOLL.                
001500*               FÖR DESSA SKAPAS EN UPPDATERINGSTRANS MED KDVTH=0         
001600*             - ALLA UPPDATERINGSTRANSAR KOMPLETTERAS MED RANDOM-         
001700*               KEY OCH SORTERAS SEDAN PÅ DENNA.                          
001800*    INDATA:                                                              
001900*                W54031                                                   
002000*    UTDATA:                                                              
002100*                W54030                                                   
002200*    SUBPROGRAM:                                                          
002300*                W510VTH                                                  
002400*                W015RAND                                                 
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700 INPUT-OUTPUT SECTION.                                                    
002800 FILE-CONTROL.                                                            
002900     SELECT W54031   ASSIGN TO UT-S-W54030D1.                             
003000     SELECT W54030   ASSIGN TO UT-S-W54030D2.                             
003100     SELECT SORTFIL  ASSIGN TO UT-S-W54030DS.                             
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500 FD  W54031                                                               
003600     RECORDING F                                                          
003700     BLOCK CONTAINS 0.                                                    
003800*01  -COPY W54031   -PRE IN-  -L.                                         
003900     EJECT                                                                
004000 FD  W54030                                                               
004100     RECORDING F                                                          
004200     BLOCK CONTAINS 0.                                                    
004300*01  POST -COPY W54030   -PRE UT-  -L.                                    
004400     EJECT                                                                
004500 SD  SORTFIL.                                                             
004600 01  SORT-POST.                                                           
004700     03  SORT-RANDOMKEY  PIC X(4).                                        
004800     03  SORT-IDARTNR    PIC S9(9)             COMP-3.                    
004900     03  SORT-KDVTH      PIC S9                COMP-3.                    
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005201                                                                          
005210*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM               PIC X(8)    VALUE 'W5403000'.                    
005400 77  W54031-EOF          PIC X       VALUE 'N'.                           
005500 77  EOF-SW              PIC X       VALUE 'N'.                           
005600 77  NEJ                 PIC X       VALUE 'N'.                           
005700 77  JA                  PIC X       VALUE 'J'.                           
005800 01  DATABASE            PIC X(4)    VALUE 'WDK6'.                        
005900                                                                          
006000 01  SUBPROGRAM.                                                          
006100     03  ABEND           PIC X(8)    VALUE 'ABEND'.                       
006200     03  W510VTH         PIC X(8)    VALUE 'W510VTH '.                    
006300     03  POSTSUM         PIC X(8)    VALUE 'POSTSUM '.                    
006400     03  W015RAND        PIC X(8)    VALUE 'W015RAND'.                    
006410                                                                          
006420*    --- VALID IDDC CODES                                                 
006430*                                                                         
006440*01  -COPY WWDCKONS                                                       
006500                                                                          
006600 01  FILLER              PIC X(16)   VALUE 'UT-AREA'.                     
006700*01  AREA -COPY W54030 -PRE UT-                                           
006800     EJECT                                                                
006900 01  FILLER              PIC X(16)   VALUE 'ART-AREA'.                    
007000*01  AREA -COPY W54031 -PRE ART-                                          
007100     EJECT                                                                
007200 01  FILLER              PIC X(16)   VALUE 'W510VTH-AREA'.                
007300*01      -COPY W510VTH                                                    
007400     EJECT                                                                
007500 01  FILLER              PIC X(16)   VALUE 'POSTSUM-AREA'.                
007600*01      -COPY W0005 -PRE POSTSUM-                                        
007700     EJECT                                                                
007800 LINKAGE SECTION.                                                         
007900*01  -COPY W0008  -PRE HANB-                                              
008000     05  FILLER                  PIC X.                                   
008100                                                                          
008200*01  -COPY W0008  -PRE PLAA-                                              
008300     05  FILLER                  PIC X.                                   
008400     EJECT                                                                
008500 PROCEDURE DIVISION USING HANB-PCB PLAA-PCB.                              
008600                                                                          
008700     OPEN INPUT  W54031                                                   
008800          OUTPUT W54030                                                   
008900                                                                          
009000     SORT SORTFIL  ASCENDING SORT-RANDOMKEY                               
009100                             SORT-IDARTNR                                 
009200                                                                          
009300     INPUT PROCEDURE A-SKAPA-POSTER                                       
009400     OUTPUT PROCEDURE B-SKRIV-SORTERADE-UTPOSTER                          
009500                                                                          
009600     CLOSE W54031                                                         
009700           W54030                                                         
009800                                                                          
009900     MOVE 'S'           TO POSTSUM-OPKOD                                  
010000     CALL POSTSUM USING POSTSUM-PARM                                      
010100                                                                          
010200     MOVE ZERO TO RETURN-CODE                                             
010300     GOBACK                                                               
010400     .                                                                    
010500     EJECT                                                                
010600 A-SKAPA-POSTER SECTION.                                                  
010700                                                                          
010800     PERFORM S01-LAS-W54031                                               
010900     PERFORM UNTIL W54031-EOF = JA                                        
011000       IF ART-PRDIRLON NOT = ZERO OR ART-PRDMTRL NOT = ZERO OR            
011100          ART-PROVRPAL NOT = ZERO                                         
011200                                                                          
011300         MOVE WC-CDC-SE       TO VTH-IDDC                                 
011400         MOVE ART-IDARTNR     TO VTH-IDARTNR                              
011500         MOVE ZERO            TO VTH-IDFKNGRP                             
011600         MOVE ART-IDLEVNR     TO VTH-IDLEVNR                              
011700         MOVE ART-BEFT        TO VTH-BEFT                                 
011800         CALL W510VTH USING VTH-W510VTH HANB-PCB PLAA-PCB                 
011900         IF ART-KDVTH NOT = VTH-KDVTH                                     
012000           MOVE ART-IDARTNR   TO SORT-IDARTNR                             
012100           MOVE VTH-KDVTH     TO SORT-KDVTH                               
012200           PERFORM S02-CALL-RAND-RELEASE-SORTPOST                         
012300         END-IF                                                           
012400       ELSE                                                               
012500         IF ART-KDVTH NOT = ZERO                                          
012600           MOVE ART-IDARTNR   TO SORT-IDARTNR                             
012700           MOVE ZERO          TO SORT-KDVTH                               
012800           PERFORM S02-CALL-RAND-RELEASE-SORTPOST                         
012900         END-IF                                                           
013000       END-IF                                                             
013100       PERFORM S01-LAS-W54031                                             
013200     END-PERFORM                                                          
013300     .                                                                    
013400     EJECT                                                                
013500 B-SKRIV-SORTERADE-UTPOSTER SECTION.                                      
013600                                                                          
013700     MOVE NEJ TO EOF-SW                                                   
013800     PERFORM UNTIL EOF-SW = JA                                            
013900       RETURN SORTFIL                                                     
014000       AT END                                                             
014100         MOVE JA TO EOF-SW                                                
014200       NOT AT END                                                         
014300         MOVE SORT-IDARTNR  TO UT-IDARTNR                                 
014400         MOVE SORT-KDVTH    TO UT-KDVTH                                   
014500         WRITE UT-POST FROM UT-AREA                                       
014600                                                                          
014700         MOVE 'UPPD'        TO POSTSUM-TRANSTYP                           
014800         MOVE 'W54030'      TO POSTSUM-FDNAMN                             
014900         MOVE 'W54030D2'    TO POSTSUM-DDNAMN2                            
015000         CALL POSTSUM USING POSTSUM-PARM                                  
015100       END-RETURN                                                         
015200     END-PERFORM                                                          
015300     .                                                                    
015400     EJECT                                                                
015500 S01-LAS-W54031  SECTION.                                                 
015600                                                                          
015700     READ W54031 INTO ART-AREA                                            
015800     AT END                                                               
015900        MOVE JA         TO W54031-EOF                                     
016000     NOT AT END                                                           
016100        MOVE ' LB '     TO POSTSUM-TRANSTYP                               
016200        MOVE 'W54031'   TO POSTSUM-FDNAMN                                 
016300        MOVE 'W54030D1' TO POSTSUM-DDNAMN2                                
016400        CALL POSTSUM USING POSTSUM-PARM                                   
016500     END-READ                                                             
016600     .                                                                    
016700     SKIP2                                                                
016800 S02-CALL-RAND-RELEASE-SORTPOST SECTION.                                  
016900                                                                          
017000     CALL W015RAND USING SORT-IDARTNR SORT-RANDOMKEY DATABASE             
017100                                                                          
017200     RELEASE SORT-POST                                                    
017300                                                                          
017400     MOVE 'SORT'        TO POSTSUM-TRANSTYP                               
017500     MOVE 'SORTER'      TO POSTSUM-FDNAMN                                 
017600     MOVE 'W54030DS'    TO POSTSUM-DDNAMN2                                
017700     CALL POSTSUM USING POSTSUM-PARM                                      
017800     .                                                                    
