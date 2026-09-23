000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2616000.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   98/04/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        HÄMTAR SAMTLIGA IDFKNGRP OCH LÄGGER UT PÅ FIL                    
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
002302*          --- ARTIKELINF                                                 
002303     SELECT W01160                     ASSIGN TO W26160D1.                
002304     SKIP2                                                                
002305*          --- IDFKNGRP + FLFKNPRI                                        
002310     SELECT W26161                     ASSIGN TO W26160D2.                
002400     SKIP2                                                                
002500*          --- SORTERINGSFIL                                              
002600     SELECT SORTFIL                    ASSIGN TO W26160DS.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003201     SKIP3                                                                
003202 FD  W01160                                                               
003203     RECORDING       F                                                    
003204     BLOCK CONTAINS  0.                                                   
003205                                                                          
003206*01  -COPY W01160      -L.                                                
003207     SKIP3                                                                
003208 FD  W26161                                                               
003209     RECORDING       F                                                    
003210     BLOCK CONTAINS  0.                                                   
003211                                                                          
003220*01  POST -COPY W26161 -PRE  UT-  -L.                                     
003300     SKIP2                                                                
003400 SD  SORTFIL.                                                             
003501                                                                          
003510*01  POST -COPY W26161      -PRE SORT-                                    
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003801                                                                          
003810*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W2616000'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004301                                                                          
004302 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
004310     88  END-OF-W01160                       VALUE 'J'.                   
004400                                                                          
004500 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
004600     88  END-OF-SORTFIL                      VALUE 'J'.                   
004700     EJECT                                                                
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005710     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005800     SKIP2                                                                
005900*    --- PARAMETRAR TILL ABEND                                            
006000                                                                          
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
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
007006*01  AREA -COPY W01160     -PRE IN-                                       
007007     EJECT                                                                
007008 01  UT-AREA-START               PIC X(24)   VALUE                        
007009                                 'UT-AREA-START  '.                       
007010     SKIP2                                                                
007011                                                                          
007020*01  AREA -COPY W26161     -PRE UT-                                       
007101     EJECT                                                                
007102 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
007103                                  'SORTWS-AREA-START  '.                  
007104     SKIP2                                                                
007105                                                                          
007110*01  AREA -COPY W26161      -PRE SORTWS-                                  
007200 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
007300     EJECT                                                                
007400 PROCEDURE DIVISION.                                                      
007500 MAIN SECTION.                                                            
007700     SKIP2                                                                
007800                                                                          
007900     PERFORM A-INIT                                                       
008000                                                                          
008100     SORT SORTFIL ASCENDING KEY SORT-IDFKNGRP                             
008300                  INPUT PROCEDURE B-SORT-INPUT                            
008400                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
008500                                                                          
008600     IF SORT-RETURN NOT = 0                                               
008700       MOVE SORT-RETURN TO SORT-RETURN-X                                  
008800       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
008900       DELIMITED BY SIZE INTO FELTEXT-STR                                 
009000       DISPLAY FELTEXT                                                    
009100       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
009200       PERFORM S99-ABEND                                                  
009300     ELSE                                                                 
009400       PERFORM Z-FINIT                                                    
009500                                                                          
009600       MOVE ZERO TO RETURN-CODE                                           
009700       GOBACK                                                             
009800     END-IF                                                               
009900                                                                          
010000     .                                                                    
010100     EJECT                                                                
010200 A-INIT SECTION.                                                          
010301                                                                          
010310     OPEN INPUT  W01160                                                   
010401                                                                          
010410     OPEN OUTPUT W26161                                                   
010500     SKIP2                                                                
010600     ACCEPT DAGENS-DATUM  FROM DATE                                       
010710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010800     .                                                                    
010901     EJECT                                                                
010902 B-SORT-INPUT  SECTION.                                                   
010903                                                                          
010904     PERFORM S01-LAES-W01160                                              
010905     PERFORM UNTIL END-OF-W01160                                          
010907       MOVE IN-CLAG-IDFKNGRP TO SORTWS-IDFKNGRP                           
010908       MOVE 'N'              TO SORTWS-FLFKNPRI                           
010909       PERFORM S31-SORT-RELEASE                                           
010910       PERFORM S01-LAES-W01160                                            
010911     END-PERFORM                                                          
010920     .                                                                    
011000     EJECT                                                                
011100 C-SORT-OUTPUT SECTION.                                                   
011200     SKIP2                                                                
011210     MOVE ZERO TO UT-IDFKNGRP                                             
011300     PERFORM S32-SORT-RETURN                                              
011400     PERFORM UNTIL END-OF-SORTFIL                                         
011500       IF SORTWS-IDFKNGRP NOT = UT-IDFKNGRP                               
011510          MOVE SORTWS-AREA TO UT-AREA                                     
011600          PERFORM S11-SKRIV-W26161                                        
011610       END-IF                                                             
011700       PERFORM S32-SORT-RETURN                                            
011800     END-PERFORM                                                          
011900     .                                                                    
012000     EJECT                                                                
012100 Z-FINIT SECTION.                                                         
012201     CLOSE W01160                                                         
012210           W26161                                                         
012301     SKIP2                                                                
012302     MOVE 'S' TO POSTSUM-OPKOD                                            
012310     CALL POSTSUM USING POSTSUM-PARM                                      
012400     .                                                                    
012501     EJECT                                                                
012502 S01-LAES-W01160  SECTION.                                                
012503     READ W01160 INTO IN-AREA                                             
012504     AT END                                                               
012506        SET END-OF-W01160 TO TRUE                                         
012507                                                                          
012508     NOT AT END                                                           
012509        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
012510        MOVE 'W26160D1' TO POSTSUM-DDNAMN2                                
012513        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
012514        CALL POSTSUM USING POSTSUM-PARM                                   
012515     END-READ                                                             
012520     .                                                                    
012601     EJECT                                                                
012602 S11-SKRIV-W26161 SECTION.                                                
012603                                                                          
012604     WRITE UT-POST FROM UT-AREA                                           
012605                                                                          
012606     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
012607     MOVE 'W26161'   TO POSTSUM-FDNAMN                                    
012608     MOVE 'W26160D2' TO POSTSUM-DDNAMN2                                   
012609     CALL POSTSUM USING POSTSUM-PARM                                      
012610     .                                                                    
012800     EJECT                                                                
012900 S31-SORT-RELEASE  SECTION.                                               
013000                                                                          
013100     RELEASE SORT-POST FROM SORTWS-AREA                                   
013200     .                                                                    
013300     EJECT                                                                
013400 S32-SORT-RETURN  SECTION.                                                
013500                                                                          
013600     RETURN SORTFIL INTO SORTWS-AREA                                      
013700     AT END                                                               
013800         SET END-OF-SORTFIL TO TRUE                                       
013900     .                                                                    
014000     EJECT                                                                
014100 S99-ABEND SECTION.                                                       
014200                                                                          
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     CALL ABEND USING RKOD-ABEND                                          
014500     .                                                                    
