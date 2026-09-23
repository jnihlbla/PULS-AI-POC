000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1117500.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   94/01/05.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET MATCHAR W91042(0) OCH W01177(-1).                     
001100*        FIL SKAPAS TILL VOLVO TRANSPORT:                                 
001200*        - ART.FÖRÄNDR. ARTIKLAR MED STATNR. GER ÄNDRINGS-POST            
001300*                    FÖR  BEART-SVE /BEART-ENG/ IDFKNGRP/ VKART           
001400*        - NY EK > 20 FRÅN < 20 GER BORTTAGS POST                         
001500*        - NY EK < 20 FRÅN > 20 GER NYUPPLÄGGS POST                       
001600*        - NY ARTIKEL MED EK < 20 OCH STATNR = 0 GER NYUPPLÄGG            
001700*                                                                         
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- LAGERBAND IN,AKTUELL VECKAS                                
002500     SELECT W91042-NEW                 ASSIGN TO W11175D1.                
002600     SKIP2                                                                
002700*          --- LAGERBAND IN,FÖRRA VECKANS                                 
002800     SELECT W01177-OLD                 ASSIGN TO W11175D2.                
002900     SKIP2                                                                
003000*          --- FIL TILL VOLVO TRANSOIRT                                   
003100     SELECT W11176                     ASSIGN TO W11175D3.                
003200     EJECT                                                                
003300*          --- NEW/CHANGED PARTS REPORT VIA D&P                           
003400     SELECT W1117B                     ASSIGN TO W11175D4.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W91042-NEW                                                           
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  -COPY W91042       -L.                                               
004400     SKIP3                                                                
004500 FD  W01177-OLD                                                           
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800*01  -COPY W011100      -L.                                               
004900     SKIP3                                                                
005000 FD  W11176                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300*01  POST -COPY A7290B01 -PRE  UT-  -L.                                   
005400     EJECT                                                                
005500 FD  W1117B                                                               
005600     RECORDING       V                                                    
005700     BLOCK CONTAINS  0.                                                   
005800 01  UT2-POST                    PIC X(198).                              
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100     SKIP2                                                                
006200                                                                          
006300*    -- CHECKED BY WY2000                                                 
006400 77  IDPGM                       PIC X(8)    VALUE 'W1117500'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  IX                          PIC S9(3)   COMP-3 VALUE ZERO.           
006800 77  IX-MAX                      PIC S9(3)   COMP-3 VALUE +10.            
006900 77  NAMN                        PIC X(35)   VALUE                        
007000                'VOLVO CAR PARTS                  '.                      
007100                                                                          
007200 77  W91042-EOF-SW-NEW           PIC X       VALUE 'N'.                   
007300     88  END-OF-W91042-NEW                   VALUE 'J'.                   
007400                                                                          
007500 77  W01177-EOF-SW-OLD           PIC X       VALUE 'N'.                   
007600     88  END-OF-W01177-OLD                   VALUE 'J'.                   
007700                                                                          
007800 77  W1117B-HEADER-SW            PIC X       VALUE 'N'.                   
007900     88  W1117B-HEADER-YES                   VALUE 'J'.                   
008000     88  W1117B-HEADER-NO                    VALUE 'N'.                   
008100                                                                          
008200 01  KDSORT-TABELL.                                                       
008300     03  KDSORT-TAB.                                                      
008400         05  FILLER              PIC X(6) VALUE 'ST PCE'.                 
008500         05  FILLER              PIC X(6) VALUE 'SA PCE'.                 
008600         05  FILLER              PIC X(6) VALUE 'KG KGM'.                 
008700         05  FILLER              PIC X(6) VALUE 'M  MTR'.                 
008800         05  FILLER              PIC X(6) VALUE 'L  DMQ'.                 
008900         05  FILLER              PIC X(6) VALUE 'ML MMT'.                 
009000         05  FILLER              PIC X(6) VALUE 'G  GRM'.                 
009100         05  FILLER              PIC X(6) VALUE 'C2 CMK'.                 
009200         05  FILLER              PIC X(6) VALUE 'M2 MTK'.                 
009300         05  FILLER              PIC X(6) VALUE 'MM CMQ'.                 
009400     03  SORT-TAB REDEFINES KDSORT-TAB.                                   
009500         05 TAB OCCURS 10.                                                
009600            07  KDSORT-PARTS     PIC X(2).                                
009700            07  FILLER           PIC X.                                   
009800            07  KDSORT-TR        PIC X(3).                                
009900                                                                          
010000 01  IDAG.                                                                
010100     03  WS-SEKEL                PIC 9(2).                                
010200     03  DAGENS-DATUM.                                                    
010300         05  DAGENS-SEKEL        PIC 9(1).                                
010400         05  FILLER              PIC 9(5).                                
010500                                                                          
010600 01  FELTEXT.                                                             
010700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010900     EJECT                                                                
011000 01  UT2-HEADER.                                                          
011100     03  FILLER                  PIC X(11)   VALUE 'PART NUMBER'.         
011200     03  FILLER                  PIC X(01)   VALUE ';'.                   
011300     03  FILLER                  PIC X(11)   VALUE 'ACTION CODE'.         
011400     03  FILLER                  PIC X(01)   VALUE ';'.                   
011500     03  FILLER                  PIC X(11)   VALUE 'SUPPLIER NO'.         
011600     03  FILLER                  PIC X(01)   VALUE ';'.                   
011700     03  FILLER                  PIC X(09)   VALUE 'DESCR SWE'.           
011800     03  FILLER                  PIC X(01)   VALUE ';'.                   
011900     03  FILLER                  PIC X(08)   VALUE 'DESCR GB'.            
012000     03  FILLER                  PIC X(01)   VALUE ';'.                   
012100     03  FILLER                  PIC X(05)   VALUE 'U O M'.               
012200     03  FILLER                  PIC X(01)   VALUE ';'.                   
012300     03  FILLER                  PIC X(14)   VALUE                        
012400                                             'PULS COMM CODE'.            
012500     03  FILLER                  PIC X(01)   VALUE ';'.                   
012600     03  FILLER                  PIC X(16)   VALUE                        
012700                                             'LOGENT COMM CODE'.          
012800     03  FILLER                  PIC X(01)   VALUE ';'.                   
012900     03  FILLER                  PIC X(15)   VALUE                        
013000                                             'PULS C O ORIGIN'.           
013100     03  FILLER                  PIC X(01)   VALUE ';'.                   
013200     03  FILLER                  PIC X(17)   VALUE                        
013300                                             'LOGENT C O ORIGIN'.         
013400     03  FILLER                  PIC X(01)   VALUE ';'.                   
013500     03  FILLER                  PIC X(14)   VALUE                        
013600                                             'FUNCTION GROUP'.            
013700     03  FILLER                  PIC X(01)   VALUE ';'.                   
013800     03  FILLER                  PIC X(08)   VALUE 'REC DATE'.            
013900     03  FILLER                  PIC X(01)   VALUE ';'.                   
014000     03  FILLER                  PIC X(08)   VALUE 'REG DATE'.            
014100     03  FILLER                  PIC X(01)   VALUE ';'.                   
014200     03  FILLER                  PIC X(22)   VALUE                        
014300                                         'OUT OF PRODUCTION DATE'.        
014400     03  FILLER                  PIC X(01)   VALUE ';'.                   
014500     03  FILLER                  PIC X(14)   VALUE                        
014600                                             'DRAWING NUMBER'.            
014700     03  FILLER                  PIC X(01)   VALUE ';'.                   
014800 01  UT2-REC.                                                             
014900     03  UT2-IDARTNR             PIC Z(14).                               
015000     03  FILLER                  PIC X(01)   VALUE ';'.                   
015100     03  UT2-ATGARDSKOD          PIC X(01).                               
015200     03  FILLER                  PIC X(01)   VALUE ';'.                   
015300     03  UT2-IDLEVNR             PIC X(09).                               
015400     03  FILLER                  PIC X(01)   VALUE ';'.                   
015500     03  UT2-BEART-SE            PIC X(25).                               
015600     03  FILLER                  PIC X(01)   VALUE ';'.                   
015700     03  UT2-BEART-GB            PIC X(25).                               
015800     03  FILLER                  PIC X(01)   VALUE ';'.                   
015900     03  UT2-ANTALSTYP           PIC X(04).                               
016000     03  FILLER                  PIC X(01)   VALUE ';'.                   
016100     03  UT2-IDSTATNR            PIC Z(14).                               
016200     03  FILLER                  PIC X(01)   VALUE ';'.                   
016300     03  FILLER                  PIC X(01)   VALUE ';'.                   
016400     03  UT2-KDARTURS            PIC X(02).                               
016500     03  FILLER                  PIC X(01)   VALUE ';'.                   
016600     03  FILLER                  PIC X(01)   VALUE ';'.                   
016700     03  UT2-IDFKNGRP            PIC Z(04).                               
016800     03  FILLER                  PIC X(01)   VALUE ';'.                   
016900     03  UT2-TIFINLV             PIC Z(05).                               
017000     03  FILLER                  PIC X(01)   VALUE ';'.                   
017100     03  UT2-TIREGDAT            PIC Z(06).                               
017200     03  FILLER                  PIC X(01)   VALUE ';'.                   
017300     03  UT2-TIURPROD            PIC Z(04).                               
017400     03  FILLER                  PIC X(01)   VALUE ';'.                   
017500     03  UT2-IDRITN              PIC X(10).                               
017600     03  FILLER                  PIC X(01)   VALUE ';'.                   
017700 01  DYNAMISKA-SUBPROGRAM.                                                
017800*                                                                         
017900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
018100                                                                          
018200*01  -COPY W009CIA                                                        
018300     EJECT                                                                
018400*01  -COPY WWPRODSL                                                       
018500     EJECT                                                                
018600*01  -COPY W0005   -PRE  POSTSUM-                                         
018700     EJECT                                                                
018800 01  IN-AREA-NEW-START          PIC X(24)   VALUE                         
018900                                 'IN-AREA-NEW-START'.                     
019000 01  INN-AREA.                                                            
019100*   05  FILLER -COPY W91042   -PRE INN-                                   
019200     EJECT                                                                
019300 01  IN-AREA-OLD-START           PIC X(24)   VALUE                        
019400                                 'IN-AREA-OLD-START'.                     
019500 01  INO-AREA.                                                            
019600*   05  FILLER -COPY W011100  -PRE INO-                                   
019700     EJECT                                                                
019800 01  UT-AREA-START               PIC X(24)   VALUE                        
019900                                 'UT-AREA-START  '.                       
020000*01  AREA -COPY A7290B01   -PRE UT-                                       
020100     EJECT                                                                
020200 PROCEDURE DIVISION.                                                      
020300                                                                          
020400     PERFORM A-INIT                                                       
020500                                                                          
020600     PERFORM S01-LAES-W91042-NEW                                          
020700     PERFORM S02-LAES-W01177-OLD                                          
020800                                                                          
020900     PERFORM UNTIL END-OF-W91042-NEW AND                                  
021000                   END-OF-W01177-OLD                                      
021100                                                                          
021200        MOVE SPACE TO UT-ARTIKEL-AREA                                     
021300        IF INN-IDARTNR = INO-IDARTNR                                      
021400*****      TEST EK OBEROENDE AV STATNR                                    
021500           IF (INN-KDERS > 20 AND INO-KDERS < 20)                         
021600           OR (INN-KDERS < 20 AND INO-KDERS > 20)                         
021700           OR INN-KDERS-UTG > 0                                           
021800              IF INN-KDERS > 20 AND INO-KDERS < 20                        
021900              OR INN-KDERS-UTG > 0                                        
022000                                                                          
022100                 MOVE 'D' TO UT-ATGARDSKOD                                
022200                             UT2-ATGARDSKOD                               
022300                 PERFORM C-SKAPA-SKRIV-UTFIL                              
022400              ELSE                                                        
022500                 IF INN-KDERS < 20 AND INO-KDERS > 20                     
022600                    MOVE 'R' TO UT-ATGARDSKOD                             
022700                                UT2-ATGARDSKOD                            
022800                    PERFORM C-SKAPA-SKRIV-UTFIL                           
022900                 END-IF                                                   
023000              END-IF                                                      
023100           ELSE                                                           
023200              IF INN-VKART = 1 AND INN-VLARTNTO = ZERO                    
023300                 CONTINUE                                                 
023400              ELSE                                                        
023500                 IF (INN-IDFKNGRP NOT = INO-IDFKNGRP)                     
023600                 OR (INN-BEART(4) NOT = INO-BEART-ENG)                    
023700                 OR (INN-BEART(8) NOT = INO-BEART-SVE)                    
023800                 OR (INN-VKART NOT = INO-VKART)                           
024500                    MOVE 'U' TO UT-ATGARDSKOD                             
024600                                UT2-ATGARDSKOD                            
025600                    PERFORM C-SKAPA-SKRIV-UTFIL                           
025700                 END-IF                                                   
025800              END-IF                                                      
025900           END-IF                                                         
026000           PERFORM S01-LAES-W91042-NEW                                    
026100           PERFORM S02-LAES-W01177-OLD                                    
026200        ELSE                                                              
026300           IF INN-IDARTNR > INO-IDARTNR                                   
026400*****  OLD-ARTNR FINNES EJ PÅ NYA LAGERBANDET  = RENSAD ARTNR             
026500              PERFORM S02-LAES-W01177-OLD                                 
026600           ELSE                                                           
026700*****   NEW-ARTNR FINNS EJ PÅ GAMLA LAGERBANDET = NYTT ARTNR              
026800              IF INN-KDERS-UTG = 0                                        
026900              AND INN-KDERS < 20                                          
027000              AND INN-IDSTATNR(3) = ZERO                                  
027100                 MOVE 'R' TO UT-ATGARDSKOD                                
027200                             UT2-ATGARDSKOD                               
027300                 PERFORM C-SKAPA-SKRIV-UTFIL                              
027400              END-IF                                                      
027500              PERFORM S01-LAES-W91042-NEW                                 
027600           END-IF                                                         
027700        END-IF                                                            
027800     END-PERFORM                                                          
027900                                                                          
028000     PERFORM Z-FINIT                                                      
028100     MOVE ZERO TO RETURN-CODE                                             
028200     GOBACK                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 A-INIT SECTION.                                                          
028600     OPEN INPUT  W91042-NEW                                               
028700                 W01177-OLD                                               
028800     OPEN OUTPUT W11176                                                   
028900                 W1117B                                                   
029000                                                                          
029100     ACCEPT DAGENS-DATUM FROM DATE                                        
029200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029300                                                                          
029400     MOVE SPACE             TO UT-FIL-AREA                                
029500     MOVE '11'              TO UT-POSTTYP                                 
029600     MOVE '01441600'        TO UT-FILAVSID                                
029700     MOVE NAMN              TO UT-FILAVSNAMN                              
029800     MOVE '5560743089'      TO UT-AVSORGNR                                
029900     IF DAGENS-SEKEL = 9                                                  
030000        MOVE 19 TO WS-SEKEL                                               
030100     ELSE                                                                 
030200        MOVE 20 TO WS-SEKEL                                               
030300     END-IF                                                               
030400     MOVE IDAG              TO UT-FILDATUM                                
030500     MOVE '2000'            TO UT-FILTID                                  
030600     PERFORM S03-SKRIV-W11176                                             
030700     MOVE '12'              TO UT-POSTTYP                                 
030800                                                                          
030900     INITIALIZE UT2-POST                                                  
031000                UT2-REC                                                   
031100     .                                                                    
031200     EJECT                                                                
031300 C-SKAPA-SKRIV-UTFIL SECTION.                                             
031400     SKIP2                                                                
031500     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
031600     MOVE INN-IDARTNR         TO CIA-IDARTBET-IN                          
031700     CALL W009CIA USING CIA-W009CIA                                       
031800     MOVE CIA-IDARTBET-UT     TO UT-ARTIKELNR                             
031900     MOVE FUNCTION TRIM(CIA-IDARTBET-UT)                                  
032000                              TO UT2-IDARTNR                              
032100                                                                          
032200     MOVE INN-BEART(8)        TO UT-BENAMNING-SE                          
032300                                 UT2-BEART-SE                             
032400     MOVE INN-BEART(4)        TO UT-BENAMNING-GB                          
032500                                 UT2-BEART-GB                             
032600                                                                          
032700     IF INN-IDLEVNR = '8261 ' OR '9998 ' OR '9997 ' OR '9996 '            
032800       MOVE '1441'            TO UT-LEVID                                 
032900                                 UT2-IDLEVNR                              
033000     ELSE                                                                 
033100       MOVE INN-IDLEVNR       TO UT-LEVID                                 
033200                                 UT2-IDLEVNR                              
033300     END-IF                                                               
033400                                                                          
033500     COMPUTE UT-NETTOVIKT = INN-VKART / 1000                              
033600                                                                          
033700     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
033800     MOVE INN-IDFKNGRP        TO CIA-IDARTBET-IN                          
033900     CALL W009CIA USING CIA-W009CIA                                       
034000     MOVE CIA-IDARTBET-UT     TO UT-FUNKTIONSGRUPP                        
034100     MOVE FUNCTION TRIM(CIA-IDARTBET-UT)                                  
034200                              TO UT2-IDFKNGRP                             
034300                                                                          
034400     MOVE 'PCE'               TO UT-ANTALSTYP                             
034500                                 UT2-ANTALSTYP                            
034600     MOVE +1 TO IX                                                        
034700     PERFORM UNTIL IX > IX-MAX                                            
034800        IF KDSORT-PARTS(IX) = INN-KDSORT                                  
034900           MOVE KDSORT-TR(IX) TO UT-ANTALSTYP                             
035000                                 UT2-ANTALSTYP                            
035100        END-IF                                                            
035200        ADD +1 TO IX                                                      
035300     END-PERFORM                                                          
035400                                                                          
035500     MOVE INN-KDARTURS        TO UT-ARTIKELURSPRUNG                       
035600                                 UT2-KDARTURS                             
035700                                                                          
035800     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
035900     MOVE INN-KDPRODSL        TO CIA-IDARTBET-IN                          
036000     CALL W009CIA USING CIA-W009CIA                                       
036100     MOVE CIA-IDARTBET-UT     TO UT-PRODUKTKOD                            
036200                                                                          
036300     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
036400     MOVE INN-IDINK           TO UT-INKNR                                 
036500                                                                          
036600     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
036700     MOVE INN-BEFT            TO CIA-IDARTBET-IN                          
036800     CALL W009CIA USING CIA-W009CIA                                       
036900     MOVE CIA-IDARTBET-UT     TO UT-FORPACKNINGSKOD                       
037000                                                                          
037100     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
037200     MOVE INN-KDFARLIG        TO CIA-IDARTBET-IN                          
037300     CALL W009CIA USING CIA-W009CIA                                       
037400     MOVE CIA-IDARTBET-UT     TO UT-FARLIGTGODS                           
037500                                                                          
037600     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
037700     MOVE INN-IDSTATNR(3)     TO CIA-IDARTBET-IN                          
037800     CALL W009CIA USING CIA-W009CIA                                       
037900     MOVE CIA-IDARTBET-UT     TO UT-STATNR                                
038000     MOVE FUNCTION TRIM(CIA-IDARTBET-UT)                                  
038100                              TO UT2-IDSTATNR                             
038200                                                                          
038300     MOVE SPACE               TO UT-OVRIGT                                
038400                                                                          
038500     MOVE INN-TIFINLV         TO UT2-TIFINLV                              
038600     MOVE INN-TIREGDAT        TO UT2-TIREGDAT                             
038700     MOVE INN-TIURPROD        TO UT2-TIURPROD                             
038800     MOVE INN-IDRITN          TO UT2-IDRITN                               
038900                                                                          
039000     MOVE INN-KDPRODSL        TO TEST-KDPRODSL                            
039100     IF KDPRODSL-LOCAL OR INN-KDSORT = 'SW'                               
039200       CONTINUE                                                           
039300     ELSE                                                                 
039400       PERFORM S03-SKRIV-W11176                                           
039500     END-IF                                                               
039600                                                                          
039700     IF INN-FLLSRDEL = 'N' OR                                             
039800        KDPRODSL-LOCAL OR                                                 
039900        INN-KDSORT = 'SW' OR                                              
040000        INN-KDERS = 21 OR 22 OR 24 OR 25 OR 29 OR 52                      
040100       CONTINUE                                                           
040200     ELSE                                                                 
040300       IF W1117B-HEADER-NO                                                
040400         MOVE UT2-HEADER         TO UT2-POST                              
040500         PERFORM S04-SKRIV-W1117B                                         
040600         SET W1117B-HEADER-YES   TO TRUE                                  
040700         MOVE SPACES             TO UT2-POST                              
040800       END-IF                                                             
040900       MOVE UT2-REC              TO UT2-POST                              
041000       PERFORM S04-SKRIV-W1117B                                           
041100       INITIALIZE UT2-REC UT2-POST                                        
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 Z-FINIT SECTION.                                                         
041600     CLOSE W91042-NEW                                                     
041700           W01177-OLD                                                     
041800           W11176                                                         
041900           W1117B                                                         
042000                                                                          
042100     MOVE 'S' TO POSTSUM-OPKOD                                            
042200     CALL POSTSUM USING POSTSUM-PARM                                      
042300     .                                                                    
042400     EJECT                                                                
042500 S01-LAES-W91042-NEW SECTION.                                             
042600     READ W91042-NEW INTO INN-AREA                                        
042700     AT END                                                               
042800        MOVE 999999999 TO INN-IDARTNR                                     
042900        SET END-OF-W91042-NEW TO TRUE                                     
043000                                                                          
043100     NOT AT END                                                           
043200        MOVE 'W91042-NEW' TO POSTSUM-FDNAMN                               
043300        MOVE 'W11175D1'   TO POSTSUM-DDNAMN2                              
043400        MOVE 'LB N'       TO POSTSUM-TRANSTYP                             
043500        CALL POSTSUM USING POSTSUM-PARM                                   
043600     END-READ                                                             
043700     .                                                                    
043800     EJECT                                                                
043900 S02-LAES-W01177-OLD SECTION.                                             
044000     READ W01177-OLD INTO INO-AREA                                        
044100     AT END                                                               
044200        MOVE 999999999 TO INO-IDARTNR                                     
044300        SET END-OF-W01177-OLD TO TRUE                                     
044400                                                                          
044500     NOT AT END                                                           
044600        MOVE 'W01177-OLD' TO POSTSUM-FDNAMN                               
044700        MOVE 'W11175D2'   TO POSTSUM-DDNAMN2                              
044800        MOVE 'LB O'       TO POSTSUM-TRANSTYP                             
044900        CALL POSTSUM USING POSTSUM-PARM                                   
045000     END-READ                                                             
045100     .                                                                    
045200     EJECT                                                                
045300 S03-SKRIV-W11176 SECTION.                                                
045400                                                                          
045500     WRITE UT-POST  FROM UT-AREA                                          
045600                                                                          
045700     MOVE 'W11176'   TO POSTSUM-FDNAMN                                    
045800     MOVE 'W11175D3' TO POSTSUM-DDNAMN2                                   
045900     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
046000     CALL POSTSUM USING POSTSUM-PARM                                      
046100     .                                                                    
046200 S04-SKRIV-W1117B SECTION.                                                
046300                                                                          
046400     WRITE UT2-POST                                                       
046500                                                                          
046600     MOVE 'W1117B'   TO POSTSUM-FDNAMN                                    
046700     MOVE 'W11175D4' TO POSTSUM-DDNAMN2                                   
046800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
046900     CALL POSTSUM USING POSTSUM-PARM                                      
047000     .                                                                    
