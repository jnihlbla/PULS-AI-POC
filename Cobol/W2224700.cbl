000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2224700.                                                
000300 AUTHOR.         ANDREASSON STEFAN.                                       
000400 DATE-WRITTEN.   00/04/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        MATCHAR LAGERBAND FÖR ATT SE OM ERS. KODEN HAR ÄNDRATS           
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
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
002301     SKIP2                                                                
002302*          --- LAGERBAND                                                  
002303     SELECT IN1                        ASSIGN TO W22247D1.                
002304     SKIP2                                                                
002305*          --- LAGERBAND                                                  
002306     SELECT IN2                        ASSIGN TO W22247D2.                
002307     SKIP2                                                                
002308*          --- ERS. KOD ÄNDRAD                                            
002310     SELECT UT                         ASSIGN TO W22247D3.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002901     SKIP3                                                                
002902 FD  IN1                                                                  
002903     RECORDING       F                                                    
002904     BLOCK CONTAINS  0.                                                   
002905                                                                          
002906*01  -COPY WCDCPART      -L.                                              
002908     SKIP3                                                                
002909 FD  IN2                                                                  
002910     RECORDING       F                                                    
002911     BLOCK CONTAINS  0.                                                   
002912                                                                          
002913*01  -COPY WCDCPART      -L.                                              
002914     SKIP3                                                                
002915 FD  UT                                                                   
002916     RECORDING       F                                                    
002917     BLOCK CONTAINS  0.                                                   
002918                                                                          
002920*01  POST -COPY W22247 -PRE UT-   -L.                                     
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W2224700'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003701                                                                          
003702 77  IN1-EOF-SW                  PIC X       VALUE 'N'.                   
003703     88  END-OF-IN1                          VALUE 'J'.                   
003704                                                                          
003705 77  IN2-EOF-SW                  PIC X       VALUE 'N'.                   
003710     88  END-OF-IN2                          VALUE 'J'.                   
003800     EJECT                                                                
003900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004000 01  FILLER REDEFINES DAGENS-DATUM.                                       
004100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004400     EJECT                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004810     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004900     SKIP2                                                                
005000*    --- PARAMETRAR TILL ABEND                                            
005100                                                                          
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005901     EJECT                                                                
005902*    --- PARAMETRAR TILL POSTSUM                                          
005903*                                                                         
005910*01  -COPY W0005   -PRE  POSTSUM-                                         
006101     EJECT                                                                
006102 01  IN1-AREA-START              PIC X(24)   VALUE                        
006103                                 'IN1-AREA-START  '.                      
006104     SKIP2                                                                
006105                                                                          
006106*01  AREA -COPY WCDCPART     -PRE IN1-                                    
006107     EJECT                                                                
006108 01  IN2-AREA-START              PIC X(24)   VALUE                        
006109                                 'IN2-AREA-START  '.                      
006110     SKIP2                                                                
006111                                                                          
006112*01  AREA -COPY WCDCPART     -PRE IN2-                                    
006113     EJECT                                                                
006114 01  UT-AREA-START               PIC X(24)   VALUE                        
006115                                 'UT-AREA-START  '.                       
006116     SKIP2                                                                
006117                                                                          
006120*01  AREA -COPY W22247       -PRE UT-                                     
006200     EJECT                                                                
006300 PROCEDURE DIVISION.                                                      
006400 MAIN SECTION.                                                            
006600     SKIP2                                                                
006700                                                                          
006800     PERFORM A-INIT                                                       
006900                                                                          
006901     PERFORM S01-LAES-IN1                                                 
006902     PERFORM S02-LAES-IN2                                                 
006903                                                                          
006904     PERFORM UNTIL END-OF-IN1                                             
006905     OR            END-OF-IN2                                             
006906       IF IN1-IDARTNR < IN2-IDARTNR                                       
006907         PERFORM S01-LAES-IN1                                             
006908       ELSE                                                               
006909         IF IN2-IDARTNR < IN1-IDARTNR                                     
006910           PERFORM S02-LAES-IN2                                           
006911         ELSE                                                             
006912           IF (IN1-KDERS = ZERO                                           
006913           AND IN2-KDERS > ZERO)                                          
006914           OR (IN1-KDERS < 20                                             
006915           AND IN2-KDERS > 20)                                            
006916             MOVE IN1-IDARTNR TO UT-IDARTNR                               
006919             PERFORM S11-SKRIV-UT                                         
006920           END-IF                                                         
006921           PERFORM S01-LAES-IN1                                           
006922           PERFORM S02-LAES-IN2                                           
006924         END-IF                                                           
006925       END-IF                                                             
006926     END-PERFORM                                                          
006927                                                                          
008100     PERFORM Z-FINIT                                                      
008200                                                                          
008300     MOVE ZERO TO RETURN-CODE                                             
008400     GOBACK                                                               
008500     .                                                                    
008600     EJECT                                                                
008700 A-INIT SECTION.                                                          
008801                                                                          
008802     OPEN INPUT  IN1                                                      
008810                 IN2                                                      
008901                                                                          
008910     OPEN OUTPUT UT                                                       
009000     SKIP2                                                                
009100     ACCEPT DAGENS-DATUM  FROM DATE                                       
009210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009300     .                                                                    
009400     EJECT                                                                
009500 Z-FINIT SECTION.                                                         
009601     CLOSE IN1                                                            
009602           IN2                                                            
009610           UT                                                             
009701     SKIP2                                                                
009702     MOVE 'S'                TO POSTSUM-OPKOD                             
009710     CALL POSTSUM USING POSTSUM-PARM                                      
009800     .                                                                    
009901     EJECT                                                                
009902 S01-LAES-IN1  SECTION.                                                   
009903     READ IN1                INTO IN1-AREA                                
009904     AT END                                                               
009905        MOVE HIGH-VALUE      TO IN1-AREA                                  
009906        SET END-OF-IN1       TO TRUE                                      
009907                                                                          
009908     NOT AT END                                                           
009909        MOVE 'IN1' TO POSTSUM-FDNAMN                                      
009910        MOVE 'W22247D1'      TO POSTSUM-DDNAMN2                           
009913        MOVE SPACE           TO POSTSUM-TRANSTYP                          
009914        CALL POSTSUM USING POSTSUM-PARM                                   
009915     END-READ                                                             
009916     .                                                                    
009917     EJECT                                                                
009918 S02-LAES-IN2  SECTION.                                                   
009919     READ IN2                INTO IN2-AREA                                
009920     AT END                                                               
009921        MOVE HIGH-VALUE      TO IN2-AREA                                  
009922        SET END-OF-IN2       TO TRUE                                      
009923                                                                          
009924     NOT AT END                                                           
009925        MOVE 'IN2'           TO POSTSUM-FDNAMN                            
009926        MOVE 'W22247D2'      TO POSTSUM-DDNAMN2                           
009929        MOVE SPACE           TO POSTSUM-TRANSTYP                          
009930        CALL POSTSUM USING POSTSUM-PARM                                   
009931     END-READ                                                             
009940     .                                                                    
010001     EJECT                                                                
010002 S11-SKRIV-UT SECTION.                                                    
010003                                                                          
010004     WRITE UT-POST           FROM UT-AREA                                 
010005                                                                          
010006     MOVE SPACE              TO POSTSUM-TRANSTYP                          
010007     MOVE 'UT'               TO POSTSUM-FDNAMN                            
010008     MOVE 'W22247D3'         TO POSTSUM-DDNAMN2                           
010009     CALL POSTSUM USING POSTSUM-PARM                                      
010010     .                                                                    
010200     EJECT                                                                
010300 S99-ABEND SECTION.                                                       
010400                                                                          
010501     SKIP2                                                                
010502     MOVE 'S'                TO POSTSUM-OPKOD                             
010510     CALL POSTSUM USING POSTSUM-PARM                                      
010600     CALL ABEND USING RKOD-ABEND                                          
010700     .                                                                    
