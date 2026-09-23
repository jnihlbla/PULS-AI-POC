000010                                                                          
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W1111000.                                                
000500*AUTHOR.         STEFAN KIHLBERG.                                         
000600*DATE-WRITTEN.   93/07/30.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001010*                                                                         
001020*        GEMENSAMMA ARTIKLAR                                              
001030*                                                                         
001100*        LÄSER W91042, SKRIVER UTFIL MED ARTIKLAR DÄR KDERS-UTG           
001200*        = 0. UTFILEN W11111 INNEHÅLLER ARTIKELNUMMER OCH                 
001300*        FLGEMART.                                                        
001400*                                                                         
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002701     SKIP2                                                                
002702*          --- ARTIKELFIL                                                 
002703     SELECT W91042                     ASSIGN TO W11110D1.                
002704     SKIP2                                                                
002705*          --- ARTIKLAR MED KDERS-UTG = 0                                 
002710     SELECT W11111                     ASSIGN TO W11110D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003301     SKIP3                                                                
003302 FD  W91042                                                               
003303     RECORDING       F                                                    
003304     BLOCK CONTAINS  0.                                                   
003305     SKIP2                                                                
003306*01  -COPY W91042      -L.                                                
003307     SKIP3                                                                
003308 FD  W11111                                                               
003309     RECORDING       F                                                    
003310     BLOCK CONTAINS  0.                                                   
003311     SKIP2                                                                
003320*01  POST -COPY W11111 -PRE  W11111-  -L.                                 
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP2                                                                
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W1111000'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004101                                                                          
004102 77  W91042-EOF-SW               PIC X       VALUE 'N'.                   
004110     88  END-OF-W91042                       VALUE 'J'.                   
004200     EJECT                                                                
004300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004400 01  FILLER REDEFINES DAGENS-DATUM.                                       
004500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004800     EJECT                                                                
004900 01  DYNAMISKA-SUBPROGRAM.                                                
005000*                                                                         
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005300     SKIP2                                                                
005400*    --- PARAMETRAR TILL ABEND                                            
005500                                                                          
005600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005800     SKIP2                                                                
005900 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006201     EJECT                                                                
006202*    --- PARAMETRAR TILL POSTSUM                                          
006203*                                                                         
006210*01  -COPY W0005   -PRE  POSTSUM-                                         
006401     EJECT                                                                
006402 01  W91042-AREA-START           PIC X(24)   VALUE                        
006403                                 'W91042-AREA-START  '.                   
006404     SKIP2                                                                
006409*01  AREA -COPY W91042      -PRE W91042-                                  
006410     EJECT                                                                
006411 01  W11111-AREA-START           PIC X(24)   VALUE                        
006412                                 'W11111-AREA-START  '.                   
006413     SKIP2                                                                
006414                                                                          
006420*01  AREA -COPY W11111     -PRE W11111-                                   
006500     EJECT                                                                
006600 PROCEDURE DIVISION.                                                      
006800     SKIP2                                                                
006900                                                                          
007000     PERFORM A-INIT                                                       
007110     PERFORM S01-LAES-W91042                                              
007200     PERFORM UNTIL END-OF-W91042                                          
007300        IF W91042-KDERS-UTG = 0                                           
007400           PERFORM B-SKAPA-UTFIL                                          
007500        END-IF                                                            
007910       PERFORM S01-LAES-W91042                                            
008000     END-PERFORM                                                          
008100                                                                          
008200                                                                          
008300     PERFORM Z-FINIT                                                      
008400                                                                          
008500     MOVE ZERO TO RETURN-CODE                                             
008600     GOBACK                                                               
008700     .                                                                    
008800     EJECT                                                                
008810                                                                          
008820                                                                          
008900 A-INIT SECTION.                                                          
009001                                                                          
009010     OPEN INPUT  W91042                                                   
009101                                                                          
009110     OPEN OUTPUT W11111                                                   
009200     SKIP2                                                                
009300     ACCEPT DAGENS-DATUM  FROM DATE                                       
009410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009500     .                                                                    
009600     EJECT                                                                
009610                                                                          
009620                                                                          
009630 B-SKAPA-UTFIL SECTION.                                                   
009640                                                                          
009650     MOVE W91042-IDARTNR    TO W11111-IDARTNR                             
009660     MOVE W91042-FLGEMART   TO W11111-FLGEMART                            
009670     PERFORM S11-SKRIV-W11111                                             
009680     .                                                                    
009690     EJECT                                                                
009691                                                                          
009692                                                                          
009700 Z-FINIT SECTION.                                                         
009800                                                                          
009801     CLOSE W91042                                                         
009810           W11111                                                         
009901     SKIP2                                                                
009902     MOVE 'S' TO POSTSUM-OPKOD                                            
009910     CALL POSTSUM USING POSTSUM-PARM                                      
010000     .                                                                    
010101     EJECT                                                                
010102 S01-LAES-W91042  SECTION.                                                
010103     SKIP2                                                                
010104     READ W91042 INTO W91042-AREA                                         
010105     AT END                                                               
010107        SET END-OF-W91042 TO TRUE                                         
010108                                                                          
010109     NOT AT END                                                           
010110        MOVE 'W91042' TO POSTSUM-FDNAMN                                   
010111        MOVE 'W11110D1' TO POSTSUM-DDNAMN2                                
010113        CALL POSTSUM USING POSTSUM-PARM                                   
010114     END-READ                                                             
010120     .                                                                    
010201     EJECT                                                                
010202 S11-SKRIV-W11111 SECTION.                                                
010203     SKIP2                                                                
010204     WRITE W11111-POST FROM W11111-AREA                                   
010205                                                                          
010207     MOVE 'W11111' TO POSTSUM-FDNAMN                                      
010208     MOVE 'W11110D2' TO POSTSUM-DDNAMN2                                   
010209     CALL POSTSUM USING POSTSUM-PARM                                      
010210     .                                                                    
010400     EJECT                                                                
