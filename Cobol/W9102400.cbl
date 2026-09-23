000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W9102400.                                                
000400 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000500 DATE-WRITTEN.   11/11/25.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        JÄMFÖR FÖRÄNDRINGAR                                              
001100*        ERSÄTTNINGAR  ERC                                                
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002501     SKIP2                                                                
002502*          --- NY FIL ERSÄTTNINGAR TILL ERC                               
002503     SELECT W91024N                    ASSIGN TO W91024D1.                
002504     SKIP2                                                                
002505*          --- GAMMAL FIL FÖRÄNDRINGAR ERSÄTTNINGAR TILL ERC              
002506     SELECT W91024G                    ASSIGN TO W91024D2.                
002507     SKIP2                                                                
002508*          --- FÖRÄNDRINGAR ERSÄTTNINGAR TILL ERC                         
002510     SELECT W91025                     ASSIGN TO W91024D3.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003101     SKIP3                                                                
003102 FD  W91024N                                                              
003103     RECORDING       F                                                    
003104     BLOCK CONTAINS  0.                                                   
003105                                                                          
003106*01  -COPY W91024      -L.                                                
003107     SKIP3                                                                
003108 FD  W91024G                                                              
003109     RECORDING       F                                                    
003110     BLOCK CONTAINS  0.                                                   
003111                                                                          
003112*01  -COPY W91024      -L.                                                
003113     SKIP3                                                                
003114 FD  W91025                                                               
003115     RECORDING       F                                                    
003116     BLOCK CONTAINS  0.                                                   
003117                                                                          
003120*01  POST -COPY W91024 -PRE  UT-  -L.                                     
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W9102400'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003901                                                                          
003902 77  NY-W91024-EOF-SW            PIC X       VALUE 'N'.                   
003903     88  END-OF-NY-W91024                    VALUE 'J'.                   
003904                                                                          
003905 77  GL-W91024-EOF-SW            PIC X       VALUE 'N'.                   
003910     88  END-OF-GL-W91024                    VALUE 'J'.                   
004000     EJECT                                                                
004100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004200 01  FILLER REDEFINES DAGENS-DATUM.                                       
004300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004600     EJECT                                                                
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800*                                                                         
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005100     SKIP2                                                                
005200*    --- PARAMETRAR TILL ABEND                                            
005300                                                                          
005400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005700     SKIP2                                                                
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101     EJECT                                                                
006102*    --- PARAMETRAR TILL POSTSUM                                          
006103*                                                                         
006110*01  -COPY W0005   -PRE  POSTSUM-                                         
006301     EJECT                                                                
006302 01  NY-AREA-START               PIC X(24)   VALUE                        
006303                                 'NY-AREA-START  '.                       
006304     SKIP2                                                                
006305                                                                          
006306*01  AREA -COPY W91024     -PRE NY-                                       
006307     EJECT                                                                
006308 01  GL-AREA-START               PIC X(24)   VALUE                        
006309                                 'GL-AREA-START  '.                       
006310     SKIP2                                                                
006311                                                                          
006312*01  AREA -COPY W91024     -PRE GL-                                       
006313     EJECT                                                                
006314 01  UT-AREA-START               PIC X(24)   VALUE                        
006315                                 'UT-AREA-START  '.                       
006316     SKIP2                                                                
006317                                                                          
006320*01  AREA -COPY W91024     -PRE UT-                                       
006400     EJECT                                                                
006500 PROCEDURE DIVISION.                                                      
006600 MAIN SECTION.                                                            
006800     SKIP2                                                                
006900                                                                          
007000     PERFORM A-INIT                                                       
007101     PERFORM S01-LAES-W91024-NY                                           
007110     PERFORM S02-LAES-W91024-GL                                           
007200     PERFORM UNTIL END-OF-NY-W91024 AND                                   
007300                   END-OF-GL-W91024                                       
007400       IF NY-IDARTNR = GL-IDARTNR                                         
007501          PERFORM S01-LAES-W91024-NY                                      
007502          PERFORM S02-LAES-W91024-GL                                      
007510       ELSE                                                               
007600          IF NY-IDARTNR < GL-IDARTNR                                      
007601***          NY ERSÄTTNING                                                
007610             MOVE NY-IDARTNR     TO UT-IDARTNR                            
007611             MOVE NY-KDERS       TO UT-KDERS                              
007612             MOVE NY-TIERSDAT    TO UT-TIERSDAT                           
007613             MOVE NY-FLERS       TO UT-FLERS                              
007614             PERFORM S11-SKRIV-W91025                                     
007615             PERFORM S01-LAES-W91024-NY                                   
007620          ELSE                                                            
007630***          NY-IDARTNR > GL-IDARTNR                                      
007631***          GL ERSÄTTNING MED NY KDERS < 20  ELLER ART BORTTAGEN         
007632***          SKRIV UTPOST = GL   SÄTT KDERS = 99 TILLS VIDARE             
007633             MOVE GL-IDARTNR     TO UT-IDARTNR                            
007634             MOVE 99             TO UT-KDERS                              
007635             MOVE GL-TIERSDAT    TO UT-TIERSDAT                           
007636             MOVE GL-FLERS       TO UT-FLERS                              
007637             PERFORM S11-SKRIV-W91025                                     
007638             PERFORM S02-LAES-W91024-GL                                   
007700          END-IF                                                          
007800       END-IF                                                             
008000     END-PERFORM                                                          
008100                                                                          
008200                                                                          
008300     PERFORM Z-FINIT                                                      
008400                                                                          
008500     MOVE ZERO TO RETURN-CODE                                             
008600     GOBACK                                                               
008700     .                                                                    
008800     EJECT                                                                
008900 A-INIT SECTION.                                                          
009001                                                                          
009002     OPEN INPUT  W91024N                                                  
009010                 W91024G                                                  
009101                                                                          
009110     OPEN OUTPUT W91025                                                   
009200     SKIP2                                                                
009300     ACCEPT DAGENS-DATUM  FROM DATE                                       
009410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009500     .                                                                    
009600     EJECT                                                                
009700 Z-FINIT SECTION.                                                         
009801     CLOSE W91024N                                                        
009802           W91024G                                                        
009810           W91025                                                         
009901     SKIP2                                                                
009902     MOVE 'S' TO POSTSUM-OPKOD                                            
009910     CALL POSTSUM USING POSTSUM-PARM                                      
010000     .                                                                    
010101     EJECT                                                                
010102 S01-LAES-W91024-NY  SECTION.                                             
010103     READ W91024N INTO NY-AREA                                            
010104     AT END                                                               
010105***     MOVE HIGH-VALUE TO NY-AREA                                        
010106        MOVE 999999999  TO NY-IDARTNR                                     
010107        SET END-OF-NY-W91024 TO TRUE                                      
010108                                                                          
010109     NOT AT END                                                           
010110        MOVE 'W91024'   TO POSTSUM-FDNAMN                                 
010111        MOVE 'W91024D1' TO POSTSUM-DDNAMN2                                
010112        MOVE 'NY'       TO POSTSUM-TRANSTYP                               
010113        CALL POSTSUM USING POSTSUM-PARM                                   
010114     END-READ                                                             
010115     .                                                                    
010116     EJECT                                                                
010117 S02-LAES-W91024-GL  SECTION.                                             
010118     READ W91024G INTO GL-AREA                                            
010119     AT END                                                               
010120***     MOVE HIGH-VALUE TO GL-AREA                                        
010121        MOVE 999999999  TO GL-IDARTNR                                     
010122        SET END-OF-GL-W91024 TO TRUE                                      
010123                                                                          
010124     NOT AT END                                                           
010125        MOVE 'W91024'   TO POSTSUM-FDNAMN                                 
010126        MOVE 'W91024D2' TO POSTSUM-DDNAMN2                                
010127        MOVE 'GL'       TO POSTSUM-TRANSTYP                               
010128        CALL POSTSUM USING POSTSUM-PARM                                   
010129     END-READ                                                             
010130     .                                                                    
010201     EJECT                                                                
010202 S11-SKRIV-W91025 SECTION.                                                
010203                                                                          
010204     WRITE UT-POST FROM UT-AREA                                           
010205                                                                          
010206     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
010207     MOVE 'W91025'   TO POSTSUM-FDNAMN                                    
010208     MOVE 'W91024D3' TO POSTSUM-DDNAMN2                                   
010209     CALL POSTSUM USING POSTSUM-PARM                                      
010210     .                                                                    
010400     EJECT                                                                
010500 S99-ABEND SECTION.                                                       
010600                                                                          
010701     SKIP2                                                                
010702     MOVE 'S' TO POSTSUM-OPKOD                                            
010710     CALL POSTSUM USING POSTSUM-PARM                                      
010800     CALL ABEND USING RKOD-ABEND                                          
010900     .                                                                    
