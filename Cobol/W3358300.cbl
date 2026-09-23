000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3358300.                                                
000400*AUTHOR.         GAVIN STENHOLM                                           
000500*DATE-WRITTEN.   APRIL 1994.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*          PROGRAMMET LÄSER FIL INEHÅLLANDE SPRÅKINFO TILL                
001100*        MARKNADSBOLAGEN. AV DENNA FIL SKAPAS "LAGOM STOR" UTFIL          
001200*        SOM SKICKAS VIA VCOM. EV. RESTERANDE POSTER SKRIVS SOM           
001300*        NY GENERATION AV INFILEN.                                        
001400*          OM RESTERANDE POSTER FINNS LÄMNAS RETURKOD 8.RETURKODEN        
001500*        TESTAS SEDAN I JCL OCH OM DEN ÄR 8 BESTÄLLS JOBBET               
001600*        IGEN OCH DEN NYA GENERATIONEN TAS IN FÖR ATT                     
001700*        KUNNA SKICKA RESTERANDE POSTER OSV.                              
001800*                                                                         
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003200     SELECT W33581I                    ASSIGN TO W33583D2.                
003300     SKIP2                                                                
003400*          --- FIL MED FÖRÄNDR. PÅ ART.REG TILL MB                        
003500*          --- POSTER KVAR ATT SÄNDA EFTER DENNA SÄNDNING.                
003600     SELECT W33581U                    ASSIGN TO W33583D3.                
003700     SKIP2                                                                
003800*          --- FIL MED FÖRÄNDR. PÅ ART.REG TILL MB                        
003900*          --- 'DEL-FIL' ATT SÄNDA VIA VCOM                               
004000     SELECT W33581V                    ASSIGN TO W33583D4.                
004100     SKIP2                                                                
004200*          --- FIL INNEHÅLLANDE EN POST.                                  
004300*          --- DENNA POST INNEHÅLLER DET TOTALA ANTALET TRANSAR.          
004400     SELECT W33588                     ASSIGN TO W33583D5.                
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP3                                                                
004800 FILE SECTION.                                                            
005000 FD  W33581I                                                              
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300     SKIP2                                                                
005400*01  -COPY W33581  -PRE  I-    -L.                                        
005500     SKIP3                                                                
005600 FD  W33581U                                                              
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900     SKIP2                                                                
006000*01  POST -COPY W33581 -PRE  UT-  -L.                                     
006100     SKIP3                                                                
006200 FD  W33581V                                                              
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500     SKIP2                                                                
006600*01  POST -COPY W33581 -PRE  UTVCOM-  -L.                                 
006700     EJECT                                                                
006800 FD  W33588                                                               
006900     RECORDING       F                                                    
007000     BLOCK CONTAINS  0.                                                   
007100     SKIP2                                                                
007200*01  POST -COPY W33581 -PRE  ANTAL-  -L.                                  
007300     EJECT                                                                
007400 WORKING-STORAGE SECTION.                                                 
007500     SKIP2                                                                
007600                                                                          
007700*    -- CHECKED BY WY2000                                                 
007800 77  IDPGM                       PIC X(8)    VALUE 'W3358300'.            
007900 77  JA                          PIC X       VALUE 'J'.                   
008000 77  NEJ                         PIC X       VALUE 'N'.                   
008100 77  WS-POSTRAKNARE              PIC 9(5)    VALUE ZERO.                  
008110 77  WS-COUNT                    PIC 9(5)    VALUE ZERO.                  
008200 77  MAX-POSTER                  PIC 9(5)    VALUE 17000.                 
008300 77  RETURKOD                    PIC S9(2)   COMP-3 VALUE ZERO.           
008400                                                                          
008500                                                                          
008600 77  W33581I-EOF-SW              PIC X       VALUE 'N'.                   
008700     88  END-OF-W33581I                      VALUE 'J'.                   
008800     EJECT                                                                
008900 77  W33588-EOF-SW              PIC X       VALUE 'N'.                    
009000     88  END-OF-W33588                      VALUE 'J'.                    
009100     EJECT                                                                
009200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009300 01  FILLER REDEFINES DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009700     EJECT                                                                
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900*                                                                         
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     SKIP2                                                                
010300*    --- PARAMETRAR TILL ABEND                                            
010400                                                                          
010500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010700     SKIP2                                                                
010800 01  FELTEXT.                                                             
010900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL POSTSUM                                          
011300*                                                                         
011400*01  -COPY W0005   -PRE  POSTSUM-                                         
011500     EJECT                                                                
011510*01  -COPY WWPRODSL                                                       
011600 01  IN-AREA-START               PIC X(24)   VALUE                        
011700                                 'IN-AREA-START  '.                       
011800     SKIP2                                                                
011900 01  IN-AREA.                                                             
012000     03  FILLER                  PIC X(477).                              
012100*01  FILLER -COPY W33581        -PRE IN-   -RED  IN-AREA                  
012200     EJECT                                                                
012300 01  ANTAL-AREA-START               PIC X(24)   VALUE                     
012400                                 'ANTAL-AREA-START  '.                    
012500     SKIP2                                                                
012600 01  ANTAL-AREA.                                                          
012700     03  FILLER                  PIC X(477).                              
012800*01  FILLER -COPY W33581      -PRE ANTAL-   -RED  ANTAL-AREA              
012900     EJECT                                                                
013000 01  UT-AREA-START               PIC X(24)   VALUE                        
013100                                 'UT-AREA-START  '.                       
013200     SKIP2                                                                
013300 01  UT-AREA.                                                             
013400     03  FILLER                  PIC X(477).                              
013500*01  FILLER -COPY W33581      -PRE UT-   -RED  UT-AREA                    
013600     EJECT                                                                
013700 01  UTVCOM-AREA-START           PIC X(24)   VALUE                        
013800                                 'UTVCOM-AREA-START  '.                   
013900     SKIP2                                                                
014000 01  UTVCOM-AREA.                                                         
014100     03  FILLER                  PIC X(477).                              
014200*01  FILLER -COPY W33581      -PRE UTVCOM-   -RED  UTVCOM-AREA            
014300     EJECT                                                                
014400 PROCEDURE DIVISION.                                                      
014500                                                                          
014600     PERFORM A-INIT                                                       
014710     PERFORM S02-LAES-W33581I                                             
014800     PERFORM S03-LAES-W33588                                              
014900     PERFORM S10-SKRIV-W33581V-000                                        
015000     MOVE 1 TO WS-POSTRAKNARE                                             
015100                                                                          
015200     PERFORM UNTIL WS-POSTRAKNARE > MAX-POSTER OR                         
015300                   END-OF-W33581I                                         
015400                                                                          
015500       PERFORM S12-SKRIV-W33581V                                          
015600                                                                          
015601       PERFORM S02-LAES-W33581I                                           
015602       ADD 1 TO WS-POSTRAKNARE                                            
015603     END-PERFORM                                                          
015700                                                                          
016200     IF END-OF-W33581I                                                    
016300       MOVE ZERO TO RETURKOD                                              
016400     ELSE                                                                 
016500       MOVE +8   TO RETURKOD                                              
016600                                                                          
016700       PERFORM UNTIL END-OF-W33581I                                       
016800                                                                          
016900         PERFORM S11-SKRIV-W33581U                                        
017000                                                                          
017100         PERFORM S02-LAES-W33581I                                         
017200       END-PERFORM                                                        
017300     END-IF                                                               
017400                                                                          
017500     PERFORM Z-FINIT                                                      
017600                                                                          
017700     MOVE RETURKOD TO RETURN-CODE                                         
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 A-INIT SECTION.                                                          
018200                                                                          
018300     OPEN INPUT                                                           
018400                 W33581I                                                  
018500                 W33588                                                   
018600                                                                          
018700     OPEN OUTPUT W33581U                                                  
018800                 W33581V                                                  
018900                                                                          
019000     ACCEPT DAGENS-DATUM FROM DATE                                        
019100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019200     .                                                                    
019300     EJECT                                                                
019400 Z-FINIT SECTION.                                                         
019500                                                                          
019600     CLOSE                                                                
019700           W33581I                                                        
019800           W33581U                                                        
019900           W33581V                                                        
020000           W33588                                                         
020100                                                                          
020200     MOVE 'S' TO POSTSUM-OPKOD                                            
020300     CALL POSTSUM USING POSTSUM-PARM                                      
020400     .                                                                    
020500     EJECT                                                                
020600 S02-LAES-W33581I SECTION.                                                
020700                                                                          
020800     READ W33581I INTO IN-AREA                                            
020900     AT END                                                               
021000        SET END-OF-W33581I TO TRUE                                        
021100                                                                          
021200     NOT AT END                                                           
021300        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
021400        MOVE 'W33581I'   TO POSTSUM-FDNAMN                                
021500        MOVE 'W33541D2' TO POSTSUM-DDNAMN2                                
021600        CALL POSTSUM USING POSTSUM-PARM                                   
021700     END-READ                                                             
021800     .                                                                    
021900     EJECT                                                                
022000 S03-LAES-W33588  SECTION.                                                
022100                                                                          
022200     READ W33588  INTO ANTAL-AREA                                         
022300     AT END                                                               
022400        SET END-OF-W33588  TO TRUE                                        
022500                                                                          
022600     NOT AT END                                                           
022700        MOVE 'TOT'      TO POSTSUM-TRANSTYP                               
022800        MOVE 'W33588'   TO POSTSUM-FDNAMN                                 
022900        MOVE 'W33583D5' TO POSTSUM-DDNAMN2                                
023000        CALL POSTSUM USING POSTSUM-PARM                                   
023100     END-READ                                                             
023200     .                                                                    
023300     EJECT                                                                
023400 S10-SKRIV-W33581V-000 SECTION.                                           
023500                                                                          
023600     MOVE ANTAL-AREA   TO UTVCOM-AREA                                     
023700     WRITE UTVCOM-POST FROM UTVCOM-AREA                                   
023800                                                                          
023900     MOVE 'TOT'     TO POSTSUM-TRANSTYP                                   
024000     MOVE 'W33581V'   TO POSTSUM-FDNAMN                                   
024100     MOVE 'W33583D4' TO POSTSUM-DDNAMN2                                   
024200     CALL POSTSUM USING POSTSUM-PARM                                      
024300     .                                                                    
024400     EJECT                                                                
024500 S11-SKRIV-W33581U SECTION.                                               
024600     MOVE IN-AREA TO UT-AREA                                              
024700     WRITE UT-POST FROM UT-AREA                                           
024800                                                                          
024900     MOVE 'UT '      TO POSTSUM-TRANSTYP                                  
025000     MOVE 'W33581U'   TO POSTSUM-FDNAMN                                   
025100     MOVE 'W33583D3' TO POSTSUM-DDNAMN2                                   
025200     CALL POSTSUM USING POSTSUM-PARM                                      
025300     .                                                                    
025400     EJECT                                                                
025500 S12-SKRIV-W33581V SECTION.                                               
025600     MOVE IN-AREA   TO UTVCOM-AREA                                        
025700     WRITE UTVCOM-POST FROM UTVCOM-AREA                                   
025800                                                                          
025900     MOVE 'VCM'      TO POSTSUM-TRANSTYP                                  
026000     MOVE 'W33581V'   TO POSTSUM-FDNAMN                                   
026100     MOVE 'W33541D4' TO POSTSUM-DDNAMN2                                   
026200     CALL POSTSUM USING POSTSUM-PARM                                      
026300     .                                                                    
026400     EJECT                                                                
