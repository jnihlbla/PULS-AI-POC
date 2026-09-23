000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2350600.                                                
000400*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500*DATE-WRITTEN.   92/03/31.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SORTERING SAMT TILLÄGG AV IDFTG                                  
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
001910 CONFIGURATION SECTION.                                                   
001920 SPECIAL-NAMES.                                                           
001930     ALPHABET Y2000 IS X'50' THRU X'99' X'00' THRU X'49'.                 
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- LEVERANSBEDÖMNINGS-INFO                                    
002403     SELECT W23501                     ASSIGN TO W23506D1.                
002404     SKIP2                                                                
002405*          --- SORTERAD LEVERANSBEDÖMNINGS-INFO                           
002410     SELECT W23503                     ASSIGN TO W23506D2.                
002500     SKIP2                                                                
002600*          --- SORTERINGSFIL                                              
002700     SELECT SORTFIL                    ASSIGN TO W23506DS.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003301     SKIP3                                                                
003302 FD  W23501                                                               
003303     RECORDING       F                                                    
003304     BLOCK CONTAINS  0.                                                   
003305     SKIP2                                                                
003306*01  -COPY W235001      -L.                                               
003307     SKIP3                                                                
003308 FD  W23503                                                               
003309     RECORDING       F                                                    
003310     BLOCK CONTAINS  0.                                                   
003311     SKIP2                                                                
003320*01  POST -COPY W235002 -PRE  UT-  -L.                                    
003400     EJECT                                                                
003500 SD  SORTFIL.                                                             
003601     SKIP2                                                                
003610*01  POST -COPY W235002      -PRE SORT-                                   
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900     SKIP2                                                                
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W2350600'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004401                                                                          
004402 77  W23501-EOF-SW               PIC X       VALUE 'N'.                   
004410     88  END-OF-W23501                       VALUE 'J'.                   
004500     EJECT                                                                
004600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004700 01  FILLER REDEFINES DAGENS-DATUM.                                       
004800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005100     EJECT                                                                
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300*                                                                         
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005510     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005600     SKIP2                                                                
005700*    --- PARAMETRAR TILL ABEND                                            
005800                                                                          
005900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006100     SKIP2                                                                
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006501     EJECT                                                                
006502*    --- PARAMETRAR TILL POSTSUM                                          
006503*                                                                         
006510*01  -COPY W0005   -PRE  POSTSUM-                                         
006701     EJECT                                                                
006702 01  IN-AREA-START               PIC X(24)   VALUE                        
006703                                 'IN-AREA-START  '.                       
006704     SKIP2                                                                
006705                                                                          
006706*01  AREA -COPY W235001     -PRE IN-                                      
006707     EJECT                                                                
006708 01  UT-AREA-START               PIC X(24)   VALUE                        
006709                                 'UT-AREA-START  '.                       
006710     SKIP2                                                                
006711                                                                          
006720*01  AREA -COPY W235002     -PRE UT-                                      
006730                                                                          
006900 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
007000     EJECT                                                                
007100 PROCEDURE DIVISION.                                                      
007300     SKIP2                                                                
007400                                                                          
007500     PERFORM A-INIT                                                       
007600                                                                          
007700     SORT SORTFIL ASCENDING KEY SORT-IDFTG                                
007800                                SORT-IDLEVNR                              
007810                                SORT-TIAARP                               
007900                  INPUT PROCEDURE B-SORT-INPUT                            
008010                  GIVING W23503                                           
008100                                                                          
008200     IF SORT-RETURN NOT = 0                                               
008300       MOVE SORT-RETURN TO SORT-RETURN-X                                  
008400       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
008500       DELIMITED BY SIZE INTO FELTEXT-STR                                 
008600       DISPLAY FELTEXT                                                    
008700       PERFORM S99-ABEND                                                  
008800     ELSE                                                                 
008900       PERFORM Z-FINIT                                                    
009000                                                                          
009100       MOVE ZERO TO RETURN-CODE                                           
009200       GOBACK                                                             
009300     END-IF                                                               
009400                                                                          
009500     .                                                                    
009600     EJECT                                                                
009700 A-INIT SECTION.                                                          
009801                                                                          
009810     OPEN INPUT  W23501                                                   
010000     SKIP2                                                                
010100     ACCEPT DAGENS-DATUM  FROM DATE                                       
010210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010300     .                                                                    
010401     EJECT                                                                
010402 B-SORT-INPUT  SECTION.                                                   
010403     SKIP2                                                                
010404     PERFORM S01-LAES-W23501                                              
010405     PERFORM UNTIL END-OF-W23501                                          
010406       MOVE IN-KDPRODSL   TO UT-KDPRODSL                                  
010407       MOVE IN-IDLEVNR    TO UT-IDLEVNR                                   
010408       MOVE IN-DATUM      TO UT-DATUM                                     
010409       MOVE IN-DATA       TO UT-DATA                                      
010410       IF IN-KDPRODSL < 30  OR IN-KDPRODSL > 90 OR                        
010411         (IN-KDPRODSL > 70 AND IN-KDPRODSL < 75)                          
010412          MOVE 57         TO UT-IDFTG                                     
010413       ELSE                                                               
010414          MOVE 03         TO UT-IDFTG                                     
010415       END-IF                                                             
010416                                                                          
010417       PERFORM S31-SORT-RELEASE                                           
010418       PERFORM S01-LAES-W23501                                            
010419     END-PERFORM                                                          
010420     .                                                                    
010500     EJECT                                                                
010600 Z-FINIT SECTION.                                                         
010710     CLOSE W23501                                                         
010801     SKIP2                                                                
010802     MOVE 'S' TO POSTSUM-OPKOD                                            
010810     CALL POSTSUM USING POSTSUM-PARM                                      
010900     .                                                                    
011001     EJECT                                                                
011002 S01-LAES-W23501  SECTION.                                                
011003     SKIP2                                                                
011004     READ W23501 INTO IN-AREA                                             
011005     AT END                                                               
011006        MOVE HIGH-VALUE TO IN-ID                                          
011007        SET END-OF-W23501 TO TRUE                                         
011008                                                                          
011009     NOT AT END                                                           
011010        MOVE 'W23501'   TO POSTSUM-FDNAMN                                 
011011        MOVE 'W23506D1' TO POSTSUM-DDNAMN2                                
011012        MOVE 'REC'      TO POSTSUM-TRANSTYP                               
011013        CALL POSTSUM USING POSTSUM-PARM                                   
011014     END-READ                                                             
011020     .                                                                    
011300     EJECT                                                                
011400 S31-SORT-RELEASE  SECTION.                                               
011500     SKIP2                                                                
011600     RELEASE SORT-POST FROM UT-AREA                                       
011700     .                                                                    
011800     EJECT                                                                
011900 S99-ABEND SECTION.                                                       
012000                                                                          
012101     SKIP2                                                                
012102     MOVE 'S' TO POSTSUM-OPKOD                                            
012110     CALL POSTSUM USING POSTSUM-PARM                                      
012200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
012300     .                                                                    
