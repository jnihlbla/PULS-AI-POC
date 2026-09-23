000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3354600.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   93/02/03.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        DETTA PROGRAM SKAPAR EN FIL MED ARTIKEL OCH EMBLEM               
001100*        BETECKNING ÅT MB ( EN PER MARKNADSBOLAG ).                       
001200*        MATCHNING MOT LAGERBANDET FÖR ATT ENDAAST FÅ MED                 
001300*        AKTUELLA ARTIKLAR.                                               
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- IN-FIL                                                     
002800     SELECT W91028                     ASSIGN TO W33546D1.                
002900     SKIP2                                                                
003000     SELECT W01172                     ASSIGN TO W33546D2.                
003100     SKIP2                                                                
003200*          --- UT-FIL                                                     
003300     SELECT W33546                     ASSIGN TO W33546D3.                
003400     SKIP2                                                                
003500*          --- SORTERINGSFIL                                              
003600     SELECT SORTFIL                    ASSIGN TO W33546DS.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W91028                                                               
004300     LABEL RECORD    STANDARD                                             
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700*01  -COPY W91028L1        -L.                                            
004800                                                                          
004900 FD  W01172                                                               
005000     LABEL RECORD    STANDARD                                             
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300     SKIP2                                                                
005400*01  -COPY W011100        -L.                                             
005500                                                                          
005600     SKIP3                                                                
005700 FD  W33546                                                               
005800     LABEL RECORD    STANDARD                                             
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100     SKIP2                                                                
006200*01  POST -COPY W33546     -PRE  UT-  -L.                                 
006300                                                                          
006400     SKIP3                                                                
006500 SD  SORTFIL                                                              
006600     RECORD VARYING.                                                      
006700     SKIP2                                                                
006800*01  SORT-POST -COPY W91028L1   -PRE SRT-.                                
006900                                                                          
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200     SKIP2                                                                
007300                                                                          
007400*    -- CHECKED BY WY2000                                                 
007500 77  IDPGM                       PIC X(8)    VALUE 'W3354600'.            
007600 77  JA                          PIC X       VALUE 'J'.                   
007700 77  NEJ                         PIC X       VALUE 'N'.                   
007800 77  POST-RAKNARE                PIC S9(9)   VALUE +0 COMP.               
007900                                                                          
008000 01  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
008100                                                                          
008200 77  W91028-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W91028                       VALUE 'J'.                   
008400                                                                          
008500 77  W01172-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-W01172                       VALUE 'J'.                   
008700                                                                          
008800 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
008900     88  END-OF-SORTFIL                      VALUE 'J'.                   
009000     EJECT                                                                
009100 01  DAGENS-DATUM.                                                        
009200     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
009300     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
009400     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
009500     EJECT                                                                
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010000     SKIP2                                                                
010100*    --- PARAMETRAR TILL ABEND                                            
010200                                                                          
010300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010500     SKIP2                                                                
010600 01  FELTEXT.                                                             
010700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL POSTSUM                                          
011100*                                                                         
011200*01  -COPY W0005      -PRE  POSTSUM-                                      
011300     EJECT                                                                
011400*01  -COPY WWPRODSL                                                       
011500 01  IN-AREA-START               PIC X(24)   VALUE                        
011600                                 'IN-AREA-START  '.                       
011700     SKIP2                                                                
011800                                                                          
011900*01  AREA -COPY W011100        -PRE DLB-                                  
012000     EJECT                                                                
012100 01  UT-AREA-START               PIC X(24)   VALUE                        
012200                                 'UT-AREA-START  '.                       
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W33546         -PRE UT-                                   
012600     EJECT                                                                
012700 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
012800                                  'SORTWS-AREA-START  '.                  
012900     SKIP2                                                                
013000                                                                          
013100*01  AREA -COPY W91028L1        -PRE SORTWS-                              
013200     EJECT                                                                
013300 PROCEDURE DIVISION.                                                      
013400     SKIP2                                                                
013500     PERFORM A-INIT                                                       
013600                                                                          
013700     SORT SORTFIL ASCENDING SRT-IDARTNR                                   
013800                            SRT-BEEMBLEM                                  
013900     USING W91028                                                         
014000     OUTPUT PROCEDURE B-BEHANDLA-UTFIL                                    
014100                                                                          
014200     IF SORT-RETURN > ZERO                                                
014300       STRING 'FEL-RETURKOD FRÅN SORT '                                   
014400       DELIMITED BY SIZE INTO FELTEXT-STR                                 
014500       DISPLAY FELTEXT                                                    
014600       PERFORM S99-ABEND                                                  
014700     ELSE                                                                 
014800       PERFORM Z-FINIT                                                    
014900                                                                          
015000       MOVE ZERO TO RETURN-CODE                                           
015100       GOBACK                                                             
015200     END-IF                                                               
015300                                                                          
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015700                                                                          
015800     OPEN INPUT  W01172                                                   
015900     OPEN OUTPUT W33546                                                   
016000                                                                          
016100     MOVE ZERO TO POST-RAKNARE                                            
016200     ACCEPT DAGENS-DATUM  FROM DATE                                       
016300     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
016400     .                                                                    
016500     EJECT                                                                
016600 B-BEHANDLA-UTFIL SECTION.                                                
016700     SKIP2                                                                
016800     PERFORM S32-SORT-RETURN                                              
016900     PERFORM S01-LAES-W01172                                              
017000     PERFORM UNTIL END-OF-SORTFIL OR END-OF-W01172                        
017100       MOVE DLB-KDPRODSL     TO TEST-KDPRODSL                             
017200       IF SORTWS-IDARTNR = DLB-IDARTNR                                    
017300*        IF KDPRODSL-LYNK                                                 
017400*          CONTINUE                                                       
017500*        ELSE                                                             
017600           MOVE 'A'            TO UT-IDVTYP                               
017700           MOVE SORTWS-IDARTNR TO UT-IDARTNR                              
017800                                  SPAR-IDARTNR                            
017900           MOVE SORTWS-BEEMBLEM TO UT-BEEMBLEM                            
018000           PERFORM S11-SKRIV-W33546                                       
018100           PERFORM S32-SORT-RETURN                                        
018200           IF SORTWS-IDARTNR = SPAR-IDARTNR                               
018300             CONTINUE                                                     
018400           ELSE                                                           
018500             PERFORM S01-LAES-W01172                                      
018600           END-IF                                                         
018700*        END-IF                                                           
018800       ELSE                                                               
018900         IF SORTWS-IDARTNR > DLB-IDARTNR                                  
019000           PERFORM S01-LAES-W01172                                        
019100         ELSE                                                             
019200           PERFORM S32-SORT-RETURN                                        
019300         END-IF                                                           
019400       END-IF                                                             
019500                                                                          
019600     END-PERFORM                                                          
019700     PERFORM UNTIL END-OF-SORTFIL                                         
019800       PERFORM S32-SORT-RETURN                                            
019900     END-PERFORM                                                          
020000     .                                                                    
020100     EJECT                                                                
020200 Z-FINIT SECTION.                                                         
020300     CLOSE W33546                                                         
020400           W01172                                                         
020500     SKIP2                                                                
020600     MOVE POST-RAKNARE TO POSTSUM-TOTTRANS                                
020700     MOVE 'T' TO POSTSUM-OPKOD                                            
020800     CALL POSTSUM USING POSTSUM-PARM                                      
020900     DISPLAY ' ANTAL UTPOSTER ' POST-RAKNARE                              
021000     .                                                                    
021100     EJECT                                                                
021200 S01-LAES-W01172 SECTION.                                                 
021300     SKIP2                                                                
021400     READ W01172 INTO DLB-AREA                                            
021500     AT END                                                               
021600        SET END-OF-W01172 TO TRUE                                         
021700                                                                          
021800     .                                                                    
021900     EJECT                                                                
022000 S11-SKRIV-W33546 SECTION.                                                
022100     SKIP2                                                                
022200     WRITE UT-POST FROM UT-AREA                                           
022300                                                                          
022400     ADD +1 TO POST-RAKNARE                                               
022500     .                                                                    
022600     EJECT                                                                
022700 S32-SORT-RETURN  SECTION.                                                
022800     SKIP2                                                                
022900     RETURN SORTFIL INTO SORTWS-AREA                                      
023000     AT END                                                               
023100         SET END-OF-SORTFIL TO TRUE                                       
023200     .                                                                    
023300     EJECT                                                                
023400 S99-ABEND SECTION.                                                       
023500                                                                          
023600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
023700     .                                                                    
