000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2318000.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   94/08/23.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET TAR IN W23179 KONKATINERAT SKAPAT UNDER               
001000*        VECKAN FÖR BERÄKNING AV LAGERSALDO.                              
001100*        SUMMERAR OCH SKAPAR UTFIL W23180.                                
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002501     SKIP2                                                                
002502*          --- ARTIKELUPPG FÖR BERÄKN LAGERSALDO                          
002503     SELECT W23179                     ASSIGN TO W23180D1.                
002504     SKIP2                                                                
002505*          --- ARTIKELUPPG SUMMERADE/VECKA FÖR BERÄKN LAGERSALDO          
002510     SELECT W23180                     ASSIGN TO W23180D2.                
002600     SKIP2                                                                
002700*          --- SORTERINGSFIL                                              
002800     SELECT SORTFIL                    ASSIGN TO W23180DS.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003401     SKIP3                                                                
003402 FD  W23179                                                               
003403     RECORDING       F                                                    
003404     BLOCK CONTAINS  0.                                                   
003405                                                                          
003406*01  -COPY W231791A      -L.                                              
003407     SKIP3                                                                
003408 FD  W23180                                                               
003409     RECORDING       F                                                    
003410     BLOCK CONTAINS  0.                                                   
003411                                                                          
003420*01  POST -COPY W231801A -PRE  UT-  -L.                                   
003500     SKIP3                                                                
003600 SD  SORTFIL.                                                             
003710*01  POST -COPY W231791A      -PRE SORT-.                                 
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W2318000'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  WS-IDARTNR-SPAR             PIC S9(9)   VALUE ZERO COMP-3.           
004503 77  WS-KVANTAL                  PIC S9(5)   VALUE ZERO COMP-3.           
004504                                                                          
004505 77  W23179-EOF-SW               PIC X       VALUE 'N'.                   
004510     88  END-OF-W23179                       VALUE 'J'.                   
004600                                                                          
004700 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
004800     88  END-OF-SORTFIL                      VALUE 'J'.                   
004900                                                                          
005000 01  KORNINGS-VECKA              PIC 9(4)    VALUE ZERO.                  
005100 01  FILLER REDEFINES KORNINGS-VECKA.                                     
005200     03  KORNINGS-AAR            PIC 9(2).                                
005300     03  KORNINGS-VV             PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL DATKORT                                          
007200*                                                                         
007300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23180'.              
007500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007600     SKIP2                                                                
007700*01  -COPY WDATKORT                                                       
007801     EJECT                                                                
007802*    --- PARAMETRAR TILL POSTSUM                                          
007803*                                                                         
007810*01  -COPY W0005   -PRE  POSTSUM-                                         
008001     EJECT                                                                
008002 01  IN-AREA-START               PIC X(24)   VALUE                        
008003                                 'IN-AREA-START  '.                       
008004     SKIP2                                                                
008005                                                                          
008006*01  AREA -COPY W231791A     -PRE IN-                                     
008007     EJECT                                                                
008008 01  UT-AREA-START               PIC X(24)   VALUE                        
008009                                 'UT-AREA-START  '.                       
008010     SKIP2                                                                
008011                                                                          
008020*01  AREA -COPY W231801A     -PRE UT-                                     
008101     EJECT                                                                
008102 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
008103                                  'SORTWS-AREA-START  '.                  
008104     SKIP2                                                                
008105                                                                          
008110*01  AREA -COPY W231791A      -PRE SORTWS-                                
008200 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
008300     EJECT                                                                
008400 PROCEDURE DIVISION.                                                      
008700                                                                          
008800     PERFORM A-INIT                                                       
008900                                                                          
009000     SORT SORTFIL ASCENDING KEY SORT-IDARTNR                              
009210                  USING W23179                                            
009300                  OUTPUT PROCEDURE B-SORT-OUTPUT                          
009400                                                                          
009500     IF SORT-RETURN NOT = 0                                               
009600       MOVE SORT-RETURN TO SORT-RETURN-X                                  
009700       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
009800       DELIMITED BY SIZE INTO FELTEXT-STR                                 
009900       DISPLAY FELTEXT                                                    
010000       PERFORM S99-ABEND                                                  
010100     ELSE                                                                 
010200       PERFORM Z-FINIT                                                    
010300                                                                          
010400       MOVE ZERO TO RETURN-CODE                                           
010500       GOBACK                                                             
010600     END-IF                                                               
010700                                                                          
010800     .                                                                    
010900     EJECT                                                                
011000 A-INIT SECTION.                                                          
011201                                                                          
011210     OPEN OUTPUT W23180                                                   
011300                                                                          
011400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
011500     MOVE D-AAR     TO  KORNINGS-AAR                                      
011600     MOVE D-VECKA   TO  KORNINGS-VV                                       
011810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011900     .                                                                    
012000     EJECT                                                                
012100 B-SORT-OUTPUT SECTION.                                                   
012200                                                                          
012210     MOVE ZERO TO WS-IDARTNR-SPAR                                         
012220     PERFORM BA-NOLLSTALL                                                 
012250                                                                          
012300     PERFORM S31-SORT-RETURN                                              
012400     PERFORM UNTIL END-OF-SORTFIL                                         
012500       IF SORTWS-IDARTNR = WS-IDARTNR-SPAR                                
012501          IF SORTWS-TIAAVV = KORNINGS-VECKA                               
012503             MOVE SORTWS-IDARTNR  TO UT-IDARTNR                           
012504             MOVE SORTWS-TIAAVV   TO UT-TIAAVV                            
012505             ADD +1               TO WS-KVANTAL                           
012506             ADD SORTWS-KVLS-C1   TO UT-KVLS-TOT                          
012507             ADD SORTWS-KVLS-C2   TO UT-KVLS-TOT                          
012508             ADD SORTWS-KVRESS-C1 TO UT-KVRESS-TOT                        
012509             ADD SORTWS-KVRESS-C2 TO UT-KVRESS-TOT                        
012510             ADD SORTWS-KVOKS-C1  TO UT-KVOKS-TOT                         
012511             ADD SORTWS-KVOKS-C2  TO UT-KVOKS-TOT                         
012512          END-IF                                                          
012513       ELSE                                                               
012514          IF UT-IDARTNR = ZERO                                            
012515             CONTINUE                                                     
012516          ELSE                                                            
012517             MOVE WS-KVANTAL TO UT-KVANTAL                                
012518             PERFORM S11-SKRIV-W23180                                     
012519          END-IF                                                          
012520          PERFORM BA-NOLLSTALL                                            
012521          MOVE SORTWS-IDARTNR TO WS-IDARTNR-SPAR                          
012522          IF SORTWS-TIAAVV = KORNINGS-VECKA                               
012523             MOVE SORTWS-IDARTNR  TO UT-IDARTNR                           
012524             MOVE SORTWS-TIAAVV   TO UT-TIAAVV                            
012525             ADD +1               TO WS-KVANTAL                           
012526             ADD SORTWS-KVLS-C1   TO UT-KVLS-TOT                          
012527             ADD SORTWS-KVLS-C2   TO UT-KVLS-TOT                          
012528             ADD SORTWS-KVRESS-C1 TO UT-KVRESS-TOT                        
012529             ADD SORTWS-KVRESS-C2 TO UT-KVRESS-TOT                        
012530             ADD SORTWS-KVOKS-C1  TO UT-KVOKS-TOT                         
012531             ADD SORTWS-KVOKS-C2  TO UT-KVOKS-TOT                         
012532          END-IF                                                          
012540       END-IF                                                             
012700       PERFORM S31-SORT-RETURN                                            
012800     END-PERFORM                                                          
012801                                                                          
012810     IF UT-IDARTNR = ZERO                                                 
012820        CONTINUE                                                          
012830     ELSE                                                                 
012831        MOVE WS-KVANTAL TO UT-KVANTAL                                     
012840        PERFORM S11-SKRIV-W23180                                          
012850     END-IF                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 BA-NOLLSTALL SECTION.                                                    
013110                                                                          
013120     MOVE ZERO TO UT-IDARTNR                                              
013130                  UT-TIAAVV                                               
013131                  UT-KVANTAL                                              
013140                  UT-KVLS-TOT                                             
013150                  UT-KVRESS-TOT                                           
013160                  UT-KVOKS-TOT                                            
013161                  WS-KVANTAL                                              
013170     .                                                                    
013180     EJECT                                                                
013200 Z-FINIT SECTION.                                                         
013201                                                                          
013210     CLOSE W23180                                                         
013301                                                                          
013302     MOVE 'S' TO POSTSUM-OPKOD                                            
013310     CALL POSTSUM USING POSTSUM-PARM                                      
013400     .                                                                    
013601     EJECT                                                                
013602 S11-SKRIV-W23180 SECTION.                                                
013603                                                                          
013604     WRITE UT-POST FROM UT-AREA                                           
013605                                                                          
013606     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
013607     MOVE 'W23180'   TO POSTSUM-FDNAMN                                    
013608     MOVE 'W23180D2' TO POSTSUM-DDNAMN2                                   
013609     CALL POSTSUM USING POSTSUM-PARM                                      
013610     .                                                                    
013800     EJECT                                                                
013900 S31-SORT-RETURN  SECTION.                                                
014000                                                                          
014100     RETURN SORTFIL INTO SORTWS-AREA                                      
014200     AT END                                                               
014300         SET END-OF-SORTFIL TO TRUE                                       
014400     .                                                                    
014500     EJECT                                                                
014600 S99-ABEND SECTION.                                                       
014700                                                                          
014802     MOVE 'S' TO POSTSUM-OPKOD                                            
014810     CALL POSTSUM USING POSTSUM-PARM                                      
014900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
015000     .                                                                    
