000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1140900.                                                
000300 AUTHOR.         EGHOLT CONNY.                                            
000400 DATE-WRITTEN.   05/10/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SORTERAR INFILEN OCH                                             
000910*        RÄTTAR ARTIKELNUMRETS FORMAT TILL ETT HELT NUMERISKT.            
000911*                                                                         
000920*        Det kan förekomma blandat IDARTNR med inledande SPACE            
000930*        och IDARTNR med inledande ZEROES.                                
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
002300     SKIP2                                                                
002400*          --- FIL FRÅN KDP MED ALLA MPNR-VOLVO-ARTIKLAR                  
002500     SELECT W11410                     ASSIGN TO W11409D1.                
002600     SKIP2                                                                
002700*          --- SORTERAD OCH FIXAD                                         
002800     SELECT W11410S                    ASSIGN TO W11409D2.                
002900     SKIP2                                                                
003000*          --- SORTERINGSFIL                                              
003100     SELECT SORTFIL                    ASSIGN TO W11409DS.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W11410                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000*01       -COPY W11410      -PRE IN-   -L.                                
004100                                                                          
004200     SKIP3                                                                
004300 FD  W11410S                                                              
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600*01  POST -COPY W11410      -PRE  UT-  -L.                                
004700                                                                          
004800     SKIP2                                                                
004900 SD  SORTFIL.                                                             
005100*01  POST -COPY W11410      -PRE SORT-                                    
005110                                                                          
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500 77  IDPGM                       PIC X(8)    VALUE 'W1140900'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800                                                                          
005900 77  W11410-EOF-SW                PIC X       VALUE 'N'.                  
006000     88  END-OF-W11410                        VALUE 'J'.                  
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700     EJECT                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900*                                                                         
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     SKIP2                                                                
007300*    --- PARAMETRAR TILL ABEND                                            
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007800     SKIP2                                                                
007900 01  FELTEXT.                                                             
008000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700 01  IN-AREA-START               PIC X(24)   VALUE                        
008800                                 'IN-AREA-START  '.                       
008900*01  AREA -COPY W11410     -PRE IN-                                       
009000     EJECT                                                                
009100                                                                          
009200                                                                          
009300                                                                          
009400 01  UT-AREA-START               PIC X(24)   VALUE                        
009500                                 'UT-AREA-START  '.                       
009600*01  AREA -COPY W11410     -PRE UT-                                       
009700     EJECT                                                                
009710                                                                          
009720                                                                          
009730                                                                          
009800 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
009900                                  'SORTWS-AREA-START  '.                  
010200*01  AREA -COPY W11410      -PRE SORTWS-                                  
010210                                                                          
010220                                                                          
010300 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
010400     EJECT                                                                
010410                                                                          
010420                                                                          
010430                                                                          
010500 PROCEDURE DIVISION.                                                      
010600 MAIN SECTION.                                                            
010700     SKIP2                                                                
010800                                                                          
010900     PERFORM A-INIT                                                       
011000                                                                          
011100     SORT SORTFIL ON                                                      
011101                  ASCENDING KEY SORT-IDPTYP                               
011110                  ASCENDING KEY SORT-IDARTNR                              
011200                 DESCENDING KEY SORT-FLGEMFMC                             
011210                 DESCENDING KEY SORT-DAREGFMC                             
011220                 DESCENDING KEY SORT-TIREGFMC                             
011230                                                                          
011300          INPUT PROCEDURE B-SORT-INPUT GIVING W11410S                     
011500                                                                          
011600     IF SORT-RETURN NOT = 0                                               
011700       MOVE SORT-RETURN TO SORT-RETURN-X                                  
011800       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
011900       DELIMITED BY SIZE INTO FELTEXT-STR                                 
012000       DISPLAY FELTEXT                                                    
012100       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
012200       PERFORM S99-ABEND                                                  
012300     ELSE                                                                 
012400       PERFORM Z-FINIT                                                    
012500                                                                          
012600       MOVE ZERO TO RETURN-CODE                                           
012700       GOBACK                                                             
012800     END-IF                                                               
012900                                                                          
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013300                                                                          
013400     OPEN INPUT  W11410                                                   
013500     SKIP2                                                                
013600     ACCEPT DAGENS-DATUM  FROM DATE                                       
013700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013800     .                                                                    
013900     EJECT                                                                
014000 B-SORT-INPUT  SECTION.                                                   
014100                                                                          
014200     PERFORM S01-LAES-W11410                                              
014300     PERFORM UNTIL END-OF-W11410                                          
014520         MOVE IN-AREA TO SORTWS-AREA                                      
014523         INSPECT SORTWS-IDARTNR REPLACING LEADING SPACES BY ZEROES        
014530                                                                          
014600         PERFORM S31-SORT-RELEASE                                         
014700                                                                          
014800         PERFORM S01-LAES-W11410                                          
014900     END-PERFORM                                                          
015000     .                                                                    
015100     EJECT                                                                
015200 Z-FINIT SECTION.                                                         
015300     CLOSE W11410                                                         
015400     SKIP2                                                                
015500     MOVE 'S' TO POSTSUM-OPKOD                                            
015600     CALL POSTSUM USING POSTSUM-PARM                                      
015700     .                                                                    
015800     EJECT                                                                
015900 S01-LAES-W11410   SECTION.                                               
016000     READ W11410 INTO IN-AREA                                             
016100     AT END                                                               
016200        MOVE HIGH-VALUE TO IN-AREA                                        
016300        SET END-OF-W11410 TO TRUE                                         
016400                                                                          
016500     NOT AT END                                                           
016600        MOVE 'W11410' TO POSTSUM-FDNAMN                                   
016700        MOVE 'W11409D1' TO POSTSUM-DDNAMN2                                
017000        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
017100        CALL POSTSUM USING POSTSUM-PARM                                   
017200     END-READ                                                             
017300     .                                                                    
017400     EJECT                                                                
017500 S31-SORT-RELEASE  SECTION.                                               
017600                                                                          
017700     RELEASE SORT-POST FROM SORTWS-AREA                                   
017800     .                                                                    
017900     EJECT                                                                
018000 S99-ABEND SECTION.                                                       
018100                                                                          
018200     SKIP2                                                                
018300     MOVE 'S' TO POSTSUM-OPKOD                                            
018400     CALL POSTSUM USING POSTSUM-PARM                                      
018500     CALL ABEND USING RKOD-ABEND                                          
018600     .                                                                    
