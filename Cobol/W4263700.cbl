000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4263700.                                                
000400*AUTHOR.         EVA LUNDELL.                                             
000500*DATE-WRITTEN.   95/08/17.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        DETTA ÄR EN SORTERING FÖR BORTTAG AV DUBLETTER                   
001000*        AV KUSTERADE KONTROLLRAPPORTER                                   
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- JUSTERADE KONTROLLRAPPORTER                                
002403     SELECT W42639                     ASSIGN TO W42637D1.                
002404     SKIP2                                                                
002405*          --- SORERADE JUSTERADE KONTROLLRAPPORTER                       
002410     SELECT W42637                     ASSIGN TO W42637D2.                
002500     SKIP2                                                                
002600*          --- SORTERINGSFIL                                              
002700     SELECT SORTFIL                    ASSIGN TO W42637DS.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003301     SKIP3                                                                
003302 FD  W42639                                                               
003303     RECORDING       F                                                    
003304     BLOCK CONTAINS  0.                                                   
003305                                                                          
003306*01  -COPY W4263901      -L.                                              
003307     SKIP3                                                                
003308 FD  W42637                                                               
003309     RECORDING       F                                                    
003310     BLOCK CONTAINS  0.                                                   
003311                                                                          
003320*01  POST -COPY W4263901 -PRE  UT-  -L.                                   
003400     SKIP2                                                                
003500 SD  SORTFIL.                                                             
003601                                                                          
003610*01  POST -COPY W4263901      -PRE SORT-                                  
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W4263700'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004401                                                                          
004402 77  W42639-EOF-SW               PIC X       VALUE 'N'.                   
004410     88  END-OF-W42639                       VALUE 'J'.                   
004500                                                                          
004600 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
004700     88  END-OF-SORTFIL                      VALUE 'J'.                   
004710 77  SPAR-IDKR                   PIC 9(5)    VALUE ZERO.                  
004720 77  SPAR-IDARTNR                PIC S9(9)  COMP-3 VALUE ZERO.            
004800     EJECT                                                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005810     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     SKIP2                                                                
006000*    --- PARAMETRAR TILL ABEND                                            
006100                                                                          
006200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006400     SKIP2                                                                
006500 01  FELTEXT.                                                             
006600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006801     EJECT                                                                
006802*    --- PARAMETRAR TILL POSTSUM                                          
006803*                                                                         
006810*01  -COPY W0005   -PRE  POSTSUM-                                         
007001     EJECT                                                                
007002 01  IN-AREA-START               PIC X(24)   VALUE                        
007003                                 'IN-AREA-START  '.                       
007004     SKIP2                                                                
007005                                                                          
007006*01  AREA -COPY W4263901     -PRE IN-                                     
007007     EJECT                                                                
007008 01  UT-AREA-START               PIC X(24)   VALUE                        
007009                                 'UT-AREA-START  '.                       
007010     SKIP2                                                                
007011                                                                          
007020*01  AREA -COPY W4263901     -PRE UT-                                     
007101     EJECT                                                                
007102 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
007103                                  'SORTWS-AREA-START  '.                  
007104     SKIP2                                                                
007105                                                                          
007110*01  AREA -COPY W4263901      -PRE SORTWS-                                
007200 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
007300     EJECT                                                                
007400 PROCEDURE DIVISION.                                                      
007600     SKIP2                                                                
007700                                                                          
007800     PERFORM A-INIT                                                       
007900                                                                          
008000     SORT SORTFIL ASCENDING KEY SORT-IDKR                                 
008100                                SORT-IDARTNR                              
008200                            DUPLICATES IN ORDER                           
008210                  USING W42639                                            
008300                  OUTPUT PROCEDURE B-SORT-OUTPUT                          
008400                                                                          
008500     IF SORT-RETURN NOT = 0                                               
008600       MOVE SORT-RETURN TO SORT-RETURN-X                                  
008700       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
008800       DELIMITED BY SIZE INTO FELTEXT-STR                                 
008900       DISPLAY FELTEXT                                                    
009000       PERFORM S99-ABEND                                                  
009100     ELSE                                                                 
009200       PERFORM Z-FINIT                                                    
009300                                                                          
009400       MOVE ZERO TO RETURN-CODE                                           
009500       GOBACK                                                             
009600     END-IF                                                               
009700                                                                          
009800     .                                                                    
009900     EJECT                                                                
010000 A-INIT SECTION.                                                          
010201                                                                          
010210     OPEN OUTPUT W42637                                                   
010300     SKIP2                                                                
010400     ACCEPT DAGENS-DATUM  FROM DATE                                       
010510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010600     .                                                                    
010700     EJECT                                                                
010800 B-SORT-OUTPUT SECTION.                                                   
010900     SKIP2                                                                
011000     PERFORM S31-SORT-RETURN                                              
011100     PERFORM UNTIL END-OF-SORTFIL                                         
011101       IF (SORTWS-IDKR NOT = SPAR-IDKR)                                   
011102       OR  (SORTWS-IDARTNR NOT = SPAR-IDARTNR)                            
011200           MOVE SORTWS-IDKR TO SPAR-IDKR                                  
011201           MOVE SORTWS-IDARTNR TO SPAR-IDARTNR                            
011210           MOVE SORTWS-AREA TO UT-AREA                                    
011211           PERFORM S11-SKRIV-W42637                                       
011220       END-IF                                                             
011400       PERFORM S31-SORT-RETURN                                            
011500     END-PERFORM                                                          
011600     .                                                                    
011700     EJECT                                                                
011800 Z-FINIT SECTION.                                                         
011910     CLOSE W42637                                                         
012001     SKIP2                                                                
012002     MOVE 'S' TO POSTSUM-OPKOD                                            
012010     CALL POSTSUM USING POSTSUM-PARM                                      
012100     .                                                                    
012301     EJECT                                                                
012302 S11-SKRIV-W42637 SECTION.                                                
012303                                                                          
012304     WRITE UT-POST FROM UT-AREA                                           
012305                                                                          
012306     MOVE UT-IDKR   TO POSTSUM-TRANSTYP                                   
012307     MOVE 'W42637' TO POSTSUM-FDNAMN                                      
012308     MOVE 'W42637D2' TO POSTSUM-DDNAMN2                                   
012309     CALL POSTSUM USING POSTSUM-PARM                                      
012310     .                                                                    
012500     EJECT                                                                
012600 S31-SORT-RETURN  SECTION.                                                
012700                                                                          
012800     RETURN SORTFIL INTO SORTWS-AREA                                      
012900     AT END                                                               
013000         SET END-OF-SORTFIL TO TRUE                                       
013100     .                                                                    
013200     EJECT                                                                
013300 S99-ABEND SECTION.                                                       
013400                                                                          
013501     SKIP2                                                                
013502     MOVE 'S' TO POSTSUM-OPKOD                                            
013510     CALL POSTSUM USING POSTSUM-PARM                                      
013600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
013700     .                                                                    
