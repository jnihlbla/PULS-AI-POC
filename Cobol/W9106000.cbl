000100                                                                          
000200                                                                          
000300 ID DIVISION.                                                             
000400     SKIP2                                                                
000500 PROGRAM-ID.     W9106000.                                                
000600*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000700*DATE-WRITTEN.   95/02/24.                                                
000800                                                                          
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        FÖRÄNDRINGAR I AFTERSALES ARTIKELDATABAS                         
001200*        SKICKAS ÖVER TILL TRANSPORT                                      
001300*                                                                         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- LAGERBAND IN,AKTUELL VECKAS                                
002800     SELECT W01177-A                   ASSIGN TO W91060D1.                
002900     SKIP2                                                                
003000*          --- LAGERBAND IN,FÖRRA VECKANS                                 
003100     SELECT W01177-B                   ASSIGN TO W91060D2.                
003200     SKIP2                                                                
003300*          --- ARTIKELFÖRÄNDRINGAR                                        
003400     SELECT W91061                     ASSIGN TO W91060D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W01177-A                                                             
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W011100      -L.                                               
004500     SKIP3                                                                
004600 FD  W01177-B                                                             
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W011100      -L.                                               
005100     SKIP3                                                                
005200 FD  W91061                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600*01  POST -COPY A7290B01 -PRE  UT-  -L.                                   
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200 77  IDPGM                       PIC X(8)    VALUE 'W9106000'.            
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500 77  IY                          PIC S9(3)   VALUE ZERO.                  
006600 77  IX                          PIC S9(3)   VALUE ZERO.                  
006700 77  IX-MAX                      PIC S9(3)   VALUE +10.                   
006800 77  LEVNR-NY-RAETT              PIC X.                                   
006900                                                                          
007000 77  W01177-EOF-SW-A             PIC X       VALUE 'N'.                   
007100     88  END-OF-W01177-A                     VALUE 'J'.                   
007200                                                                          
007300 77  W01177-EOF-SW-B             PIC X       VALUE 'N'.                   
007400     88  END-OF-W01177-B                     VALUE 'J'.                   
007500                                                                          
007600 77  SW-MATCH-INB                PIC X       VALUE 'N'.                   
007700 77  SW-PARMA-GSDB                PIC X       VALUE 'N'.                  
007800     SKIP3                                                                
007900 01  WS-GRP-PARAMETER        PIC S9(4) COMP-3.                            
008000     88 GRP-IDFKNGRP         VALUE 1960 THRU 1999.                        
008100                                                                          
008200 01  WS-PARMA-GSDB.                                                       
008300     03  WSA-IDLEVNR             PIC X(5)    VALUE SPACE.                 
008400     03  WSB-IDLEVNR             PIC X(5)    VALUE SPACE.                 
008500 01  IDAG.                                                                
008600     03  FILLER                      PIC 9(2)    VALUE 20.                
008700     03  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.              
008800     03  FILLER REDEFINES DAGENS-DATUM.                                   
008900         05  DAGENS-DATUM-AAR        PIC 9(2).                            
009000         05  DAGENS-DATUM-MAANAD     PIC 9(2).                            
009100         05  DAGENS-DATUM-DAG        PIC 9(2).                            
009200*                                                                         
009300*01  -COPY WWPRODSL                                                       
009400     SKIP3                                                                
009500 01  ARBETSAREOR.                                                         
009600     03  NY                      PIC X  VALUE 'R'.                        
009700     03  AENDRING                PIC X  VALUE 'U'.                        
009800     03  BORTTAG                 PIC X  VALUE 'D'.                        
009900     03  NAMN                    PIC X(35)     VALUE                      
010000         'VOLVO CAR CORPORATION, AFTERSALES'.                             
010100     03  W-FAELT                 PIC X(9).                                
010200     03  W-FAELT-NUM REDEFINES W-FAELT PIC 9(9).                          
010300                                                                          
010400 01  KDSORT-TABELL.                                                       
010500     03  KDSORT-TAB.                                                      
010600         05  FILLER             PIC X(6)  VALUE 'ST PCE'.                 
010700         05  FILLER             PIC X(6)  VALUE 'SA PCE'.                 
010800         05  FILLER             PIC X(6)  VALUE 'KG KGM'.                 
010900         05  FILLER             PIC X(6)  VALUE 'M  MTR'.                 
011000         05  FILLER             PIC X(6)  VALUE 'L  DMQ'.                 
011100         05  FILLER             PIC X(6)  VALUE 'ML MMT'.                 
011200         05  FILLER             PIC X(6)  VALUE 'G  GRM'.                 
011300         05  FILLER             PIC X(6)  VALUE 'C2 CMK'.                 
011400         05  FILLER             PIC X(6)  VALUE 'M2 MTK'.                 
011500         05  FILLER             PIC X(6)  VALUE 'MM CMQ'.                 
011600     03 SORT-TAB REDEFINES KDSORT-TAB.                                    
011700         05  TAB OCCURS 10.                                               
011800             07  KDSORT-PARTS   PIC X(2).                                 
011900             07  FILLER         PIC X.                                    
012000             07  KDSORT-TR      PIC X(3).                                 
012100     EJECT                                                                
012200 01  DYNAMISKA-SUBPROGRAM.                                                
012300*                                                                         
012400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012600     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
012700     SKIP3                                                                
012800*    --- PARAMETRAR TILL ABEND                                            
012900                                                                          
013000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013200     SKIP2                                                                
013300 01  FELTEXT.                                                             
013400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL POSTSUM                                          
013800*                                                                         
013900*01  -COPY W0005   -PRE  POSTSUM-                                         
014000     EJECT                                                                
014100*01  -COPY W009CIA                                                        
014200     EJECT                                                                
014300 01  INA-AREA-START              PIC X(24)   VALUE                        
014400                                 'INA-AREA-START  '.                      
014500     SKIP2                                                                
014600 01  INA-AREA.                                                            
014700     03  INA-AREA-0.                                                      
014800*   05  FILLER -COPY W011100  -PRE INA-                                   
014900     EJECT                                                                
015000 01  INB-AREA-START              PIC X(24)   VALUE                        
015100                                 'INB-AREA-START  '.                      
015200     SKIP2                                                                
015300 01  INB-AREA.                                                            
015400     03  INB-AREA-0.                                                      
015500*   05  FILLER -COPY W011100  -PRE INB-                                   
015600     EJECT                                                                
015700 01  UT-AREA-START               PIC X(24)   VALUE                        
015800                                 'UT-AREA-START  '.                       
015900     SKIP2                                                                
016000                                                                          
016100*01  AREA -COPY A7290B01     -PRE UT-                                     
016200     EJECT                                                                
016300 PROCEDURE DIVISION.                                                      
016400                                                                          
016500     PERFORM A-INIT                                                       
016600     PERFORM S01-LAES-W01177-A                                            
016700     PERFORM S02-LAES-W01177-B                                            
016800     PERFORM UNTIL END-OF-W01177-A AND                                    
016900                   END-OF-W01177-B                                        
017000       IF INA-IDARTNR = INB-IDARTNR                                       
017100         MOVE SPACE               TO UT-ARTIKEL-AREA                      
017200         MOVE INA-KDPRODSL        TO TEST-KDPRODSL                        
017300           IF INA-PRARTSTD > 0 AND KDPRODSL-VOLVO-BIMA                    
017400             MOVE INA-IDLEVNR TO WSA-IDLEVNR                              
017500             INSPECT WSA-IDLEVNR REPLACING ALL SPACE BY ZERO              
017600             IF (INA-IDLEVNR         NOT = INB-IDLEVNR)                   
017700*** FIX FÖR REG DATUM (9998-> ALFA SOM EJ KOMMIT MED)                     
017800             OR (DAGENS-DATUM < 061031 AND                                
017900                 INA-TIREGDAT > 030223 AND                                
018000                 INA-TIREGDAT < 061031 AND                                
018100                 WSA-IDLEVNR NOT NUMERIC AND                              
018200                 INA-KDERS = ZERO)                                        
018300              MOVE INA-IDFKNGRP TO WS-GRP-PARAMETER                       
018400              IF (GRP-IDFKNGRP                                            
018500              AND KDPRODSL-EMB)                                           
018600                CONTINUE                                                  
018700              ELSE                                                        
018800*** SLUTFIXAT                                                             
018900                PERFORM C-PARMA-GSDB                                      
019000                IF SW-PARMA-GSDB = 'J'                                    
019100                   CONTINUE                                               
019200                ELSE                                                      
019300                  IF INA-KDERS > 20                                       
019400                     CONTINUE                                             
019500                  ELSE                                                    
019600                     PERFORM S03-TESTA-LEVNR-NY                           
019700                     IF LEVNR-NY-RAETT = JA                               
019800                         MOVE NY TO UT-ATGARDSKOD                         
019900                         PERFORM B-SKRIV-FIL-TILL-TRANSPORT               
020000                     END-IF                                               
020100                  END-IF                                                  
020200                END-IF                                                    
020300              END-IF                                                      
020400             END-IF                                                       
020500           END-IF                                                         
020600         PERFORM S01-LAES-W01177-A                                        
020700         PERFORM S02-LAES-W01177-B                                        
020800       ELSE                                                               
020900         IF INA-IDARTNR > INB-IDARTNR                                     
021000***         INB FINNS EJ PÅ INA       ***    INB REDAN UTG ART            
021100            PERFORM S02-LAES-W01177-B                                     
021200         ELSE                                                             
021300***         INA FINNS EJ PÅ INB       ***    INA NY ARTIKEL               
021400           MOVE INA-KDPRODSL      TO TEST-KDPRODSL                        
021500           IF INA-PRARTSTD > ZERO AND KDPRODSL-VOLVO-BIMA                 
021600             MOVE INA-IDFKNGRP TO WS-GRP-PARAMETER                        
021700             IF (GRP-IDFKNGRP                                             
021800             AND KDPRODSL-EMB)                                            
021900               CONTINUE                                                   
022000             ELSE                                                         
022100               IF INA-KDERS > 20                                          
022200                  CONTINUE                                                
022300               ELSE                                                       
022400                  PERFORM S03-TESTA-LEVNR-NY                              
022500                  IF LEVNR-NY-RAETT = JA                                  
022600                      MOVE NY      TO UT-ATGARDSKOD                       
022700                      PERFORM B-SKRIV-FIL-TILL-TRANSPORT                  
022800                  END-IF                                                  
022900               END-IF                                                     
023000             END-IF                                                       
023100           END-IF                                                         
023200           PERFORM S01-LAES-W01177-A                                      
023300         END-IF                                                           
023400       END-IF                                                             
023500     END-PERFORM                                                          
023600                                                                          
023700                                                                          
023800     PERFORM Z-FINIT                                                      
023900                                                                          
024000     MOVE ZERO TO RETURN-CODE                                             
024100     GOBACK                                                               
024200     .                                                                    
024300     EJECT                                                                
024400 A-INIT SECTION.                                                          
024500                                                                          
024600     OPEN INPUT  W01177-A                                                 
024700                 W01177-B                                                 
024800                                                                          
024900     OPEN OUTPUT W91061                                                   
025000     SKIP2                                                                
025100     ACCEPT DAGENS-DATUM  FROM DATE                                       
025200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025300                                                                          
025400     MOVE '11'                   TO UT-POSTTYP                            
025500     MOVE SPACE                  TO UT-FIL-AREA                           
025600     MOVE '01082600'             TO UT-FILAVSID                           
025700     MOVE NAMN                   TO UT-FILAVSNAMN                         
025800     MOVE '5560743089'           TO UT-AVSORGNR                           
025900     MOVE IDAG                   TO UT-FILDATUM                           
026000     MOVE '2000'                 TO UT-FILTID                             
026100                                                                          
026200        PERFORM S11-SKRIV-W91061                                          
026300                                                                          
026400     MOVE '12'                   TO UT-POSTTYP                            
026500     .                                                                    
026600     EJECT                                                                
026700 B-SKRIV-FIL-TILL-TRANSPORT SECTION.                                      
026800                                                                          
026900        PERFORM BA-REDIGERA                                               
027000                                                                          
027100        PERFORM S11-SKRIV-W91061                                          
027200     .                                                                    
027300     EJECT                                                                
027400 BA-REDIGERA                SECTION.                                      
027500                                                                          
027600        MOVE 'VO'                TO CIA-IDARTPRE-IN                       
027700        MOVE INA-IDARTNR         TO CIA-IDARTBET-IN                       
027800        CALL W009CIA USING          CIA-W009CIA                           
027900        MOVE CIA-IDARTBET-UT     TO UT-ARTIKELNR                          
028000                                                                          
028100        MOVE INA-BEART-SVE       TO UT-BENAMNING-SE                       
028200        MOVE INA-BEART-ENG       TO UT-BENAMNING-GB                       
028300                                                                          
028400        MOVE INA-IDLEVNR         TO UT-LEVID                              
028500                                                                          
028600        COMPUTE UT-NETTOVIKT =   INA-VKART / 1000                         
028700                                                                          
028800        MOVE 'VO'                TO CIA-IDARTPRE-IN                       
028900        MOVE INA-IDFKNGRP        TO CIA-IDARTBET-IN                       
029000        CALL W009CIA USING          CIA-W009CIA                           
029100        MOVE CIA-IDARTBET-UT     TO UT-FUNKTIONSGRUPP                     
029200                                                                          
029300        MOVE 'PCE'               TO UT-ANTALSTYP                          
029400        MOVE +1 TO IX                                                     
029500        PERFORM UNTIL IX > IX-MAX                                         
029600          IF KDSORT-PARTS(IX) = INA-KDSORT                                
029700             MOVE KDSORT-TR(IX) TO UT-ANTALSTYP                           
029800          END-IF                                                          
029900          ADD +1 TO IX                                                    
030000        END-PERFORM                                                       
030100                                                                          
030200        MOVE INA-KDARTURS      TO UT-ARTIKELURSPRUNG                      
030300                                                                          
030400        MOVE 'VO'                TO CIA-IDARTPRE-IN                       
030500        MOVE INA-KDPRODSL        TO CIA-IDARTBET-IN                       
030600        CALL W009CIA USING          CIA-W009CIA                           
030700        MOVE CIA-IDARTBET-UT     TO UT-PRODUKTKOD                         
030800                                                                          
030900        MOVE 'VO'                TO CIA-IDARTPRE-IN                       
031000        MOVE INA-IDINK           TO UT-INKNR                              
031100                                                                          
031200        MOVE 'VO'                TO CIA-IDARTPRE-IN                       
031300        MOVE INA-BEFT            TO CIA-IDARTBET-IN                       
031400        CALL W009CIA USING          CIA-W009CIA                           
031500        MOVE CIA-IDARTBET-UT     TO UT-FORPACKNINGSKOD                    
031600                                                                          
031700        MOVE 'VO'                TO CIA-IDARTPRE-IN                       
031800        MOVE INA-KDFARLIG        TO CIA-IDARTBET-IN                       
031900        CALL W009CIA USING          CIA-W009CIA                           
032000        MOVE CIA-IDARTBET-UT     TO UT-FARLIGTGODS                        
032100                                                                          
032200        MOVE SPACE               TO UT-STATNR                             
032300        MOVE SPACE               TO UT-OVRIGT                             
032400                                                                          
032500     .                                                                    
032600     EJECT                                                                
032700 C-PARMA-GSDB SECTION.                                                    
032800                                                                          
032900     MOVE NEJ         TO SW-PARMA-GSDB                                    
033000     MOVE INA-IDLEVNR TO WSA-IDLEVNR                                      
033100     INSPECT WSA-IDLEVNR REPLACING ALL SPACE BY ZERO                      
033200     MOVE INB-IDLEVNR TO WSB-IDLEVNR                                      
033300     INSPECT WSB-IDLEVNR REPLACING ALL SPACE BY ZERO                      
033400*****IF (WSA-IDLEVNR NOT NUMERIC AND                                      
033500*        WSB-IDLEVNR NUMERIC)                                             
033600*       MOVE JA TO SW-PARMA-GSDB                                          
033700*****END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000                                                                          
034100 Z-FINIT SECTION.                                                         
034200     CLOSE W01177-A                                                       
034300           W01177-B                                                       
034400           W91061                                                         
034500     SKIP2                                                                
034600     MOVE 'S' TO POSTSUM-OPKOD                                            
034700     CALL POSTSUM USING POSTSUM-PARM                                      
034800     .                                                                    
034900     EJECT                                                                
035000 S01-LAES-W01177-A  SECTION.                                              
035100     READ W01177-A INTO INA-AREA                                          
035200     AT END                                                               
035300        MOVE 999999999      TO INA-IDARTNR                                
035400        SET END-OF-W01177-A TO TRUE                                       
035500                                                                          
035600     NOT AT END                                                           
035700        MOVE 'W01177-A' TO POSTSUM-FDNAMN                                 
035800        MOVE 'W91060D1' TO POSTSUM-DDNAMN2                                
035900        MOVE 'LB A'     TO POSTSUM-TRANSTYP                               
036000        CALL POSTSUM USING POSTSUM-PARM                                   
036100     END-READ                                                             
036200     .                                                                    
036300     EJECT                                                                
036400 S02-LAES-W01177-B  SECTION.                                              
036500     READ W01177-B INTO INB-AREA                                          
036600     AT END                                                               
036700        MOVE 999999999      TO INB-IDARTNR                                
036800        SET END-OF-W01177-B TO TRUE                                       
036900                                                                          
037000     NOT AT END                                                           
037100        MOVE 'W01177-B' TO POSTSUM-FDNAMN                                 
037200        MOVE 'W91060D2' TO POSTSUM-DDNAMN2                                
037300        MOVE 'LB F'     TO POSTSUM-TRANSTYP                               
037400        CALL POSTSUM USING POSTSUM-PARM                                   
037500     END-READ                                                             
037600     .                                                                    
037700     EJECT                                                                
037800 S03-TESTA-LEVNR-NY SECTION.                                              
037900     IF (INA-IDLEVNR = '0' OR '9996' OR '9997' OR '9998'                  
038000       OR '9999' OR '8261' OR '8265' OR 'BWLAA'                           
038100       OR 'CNT5A') OR                                                     
038200        INA-KDSORT = 'SW'                                                 
038300         MOVE NEJ TO LEVNR-NY-RAETT                                       
038400     ELSE                                                                 
038500         MOVE JA  TO LEVNR-NY-RAETT                                       
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 S11-SKRIV-W91061 SECTION.                                                
039000                                                                          
039100     WRITE UT-POST FROM UT-AREA                                           
039200                                                                          
039300     MOVE 'POST'     TO POSTSUM-TRANSTYP                                  
039400     MOVE 'W91061'   TO POSTSUM-FDNAMN                                    
039500     MOVE 'W91060D3' TO POSTSUM-DDNAMN2                                   
039600     CALL POSTSUM USING POSTSUM-PARM                                      
039700     .                                                                    
039800     EJECT                                                                
039900 S99-ABEND SECTION.                                                       
040000                                                                          
040100     SKIP2                                                                
040200     MOVE 'S' TO POSTSUM-OPKOD                                            
040300     CALL POSTSUM USING POSTSUM-PARM                                      
040400     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
040500     .                                                                    
