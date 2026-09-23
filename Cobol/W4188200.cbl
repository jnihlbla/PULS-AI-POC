000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4188200.                                                
000400 AUTHOR.         MÅNS SAMUELSSON.                                         
000500 DATE-WRITTEN.   97/02/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SORTERAR W41880 FILEN I RANDOM ORDNING INFÖR                     
001100*        UNLOAD RENSNING                                                  
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
002502*          --- FIL MED RENSNINGSNYCKLAR                                   
002503     SELECT W41880                     ASSIGN TO W41882D1.                
002504     SKIP2                                                                
002505*          --- SORTERAD FIL MED RENSNINGSNYCKLAR                          
002510     SELECT W41882                     ASSIGN TO W41882D2.                
002600     SKIP2                                                                
002700*          --- SORTERINGSFIL                                              
002800     SELECT SORTFIL                    ASSIGN TO W41882DS.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003401     SKIP3                                                                
003402 FD  W41880                                                               
003403     RECORDING       F                                                    
003404     BLOCK CONTAINS  0.                                                   
003405                                                                          
003406*01  -COPY W41880      -L.                                                
003407     SKIP3                                                                
003408 FD  W41882                                                               
003409     RECORDING       F                                                    
003410     BLOCK CONTAINS  0.                                                   
003411                                                                          
003412 01  UT-POST.                                                             
003413     03 FILLER              PIC X(4).                                     
003414*    03 -COPY W41880     -PRE UT- -L.                                     
003500     SKIP2                                                                
003600 SD  SORTFIL.                                                             
003701                                                                          
003702 01  SORT-POST.                                                           
003703     03 SORT-RANDOMKEY      PIC X(4).                                     
003710*    03 -COPY W41880      -PRE SORT-                                      
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W4188200'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  DATABAS                     PIC X(4)    VALUE 'WDA2'.                
004500 77  W-ANTAL                     PIC 9(7)    VALUE ZERO.                  
004501                                                                          
004502 77  W41880-EOF-SW               PIC X       VALUE 'N'.                   
004510     88  END-OF-W41880                       VALUE 'J'.                   
004600                                                                          
004700 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
004800     88  END-OF-SORTFIL                      VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005910     03  W015RAND                PIC X(8)    VALUE 'W015RAND'.            
006000     SKIP2                                                                
006100*    --- PARAMETRAR TILL ABEND                                            
006200                                                                          
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007101     EJECT                                                                
007202 01  IN-AREA-START               PIC X(24)   VALUE                        
007203                                 'IN-AREA-START  '.                       
007204     SKIP2                                                                
007205                                                                          
007206*01  AREA -COPY W41880     -PRE IN-                                       
007207     EJECT                                                                
007208 01  UT-AREA-START               PIC X(24)   VALUE                        
007209                                 'UT-AREA-START  '.                       
007210     SKIP2                                                                
007211                                                                          
007212 01  UT-AREA.                                                             
007213     03 UT-RANDOMKEY         PIC X(4).                                    
007214     03 -COPY W41880      -PRE UT-                                        
007301     EJECT                                                                
007302 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
007303                                  'SORTWS-AREA-START  '.                  
007304     SKIP2                                                                
007305                                                                          
007306 01  SORTWS-AREA.                                                         
007307     03 SORTWS-RANDOMKEY         PIC X(4).                                
007310     03 -COPY W41880      -PRE SORTWS-                                    
007400 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
007500     EJECT                                                                
007600 PROCEDURE DIVISION.                                                      
007700 MAIN SECTION.                                                            
007900     SKIP2                                                                
008000                                                                          
008100     PERFORM A-INIT                                                       
008200                                                                          
008300     SORT SORTFIL ASCENDING KEY SORT-RANDOMKEY                            
008500                  INPUT PROCEDURE B-SORT-INPUT                            
008600                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
008700                                                                          
008800     IF SORT-RETURN NOT = 0                                               
008900       MOVE SORT-RETURN TO SORT-RETURN-X                                  
009000       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
009100       DELIMITED BY SIZE INTO FELTEXT-STR                                 
009200       DISPLAY FELTEXT                                                    
009400       PERFORM S99-ABEND                                                  
009500     ELSE                                                                 
009600       PERFORM Z-FINIT                                                    
009700                                                                          
009800       MOVE ZERO TO RETURN-CODE                                           
009900       GOBACK                                                             
010000     END-IF                                                               
010100                                                                          
010200     .                                                                    
010300     EJECT                                                                
010400 A-INIT SECTION.                                                          
010501                                                                          
010502     DISPLAY 'A-INIT'                                                     
010510     OPEN INPUT  W41880                                                   
010601                                                                          
010610     OPEN OUTPUT W41882                                                   
010700     SKIP2                                                                
010800     ACCEPT DAGENS-DATUM  FROM DATE                                       
010910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011000     .                                                                    
011101     EJECT                                                                
011102 B-SORT-INPUT  SECTION.                                                   
011103     DISPLAY 'B-SORT'                                                     
011104     MOVE +1 TO W-ANTAL                                                   
011105                                                                          
011106     PERFORM S01-LAES-W41880                                              
011107     PERFORM UNTIL END-OF-W41880                                          
011108       MOVE IN-AREA TO SORTWS-W41880                                      
011109       CALL W015RAND USING SORTWS-IDLEVANM                                
011110                           SORTWS-RANDOMKEY DATABAS                       
011113       PERFORM S31-SORT-RELEASE                                           
011115       PERFORM S01-LAES-W41880                                            
011116       ADD +1 TO W-ANTAL                                                  
011117     END-PERFORM                                                          
011118     DISPLAY 'TILL SORT = ' W-ANTAL                                       
011120     .                                                                    
011200     EJECT                                                                
011300 C-SORT-OUTPUT SECTION.                                                   
011400     SKIP2                                                                
011410     DISPLAY 'B-SORT'                                                     
011420     MOVE +1 TO W-ANTAL                                                   
011500     PERFORM S32-SORT-RETURN                                              
011600     PERFORM UNTIL END-OF-SORTFIL                                         
011710       MOVE SORTWS-AREA   TO UT-AREA                                      
011800       PERFORM S11-SKRIV-W41882                                           
011900       PERFORM S32-SORT-RETURN                                            
011910       ADD +1 TO W-ANTAL                                                  
012000     END-PERFORM                                                          
012010     DISPLAY 'FRÅN SORT = ' W-ANTAL                                       
012100     .                                                                    
012200     EJECT                                                                
012300 Z-FINIT SECTION.                                                         
012400     DISPLAY 'Z-FINIT'                                                    
012401     CLOSE W41880                                                         
012410           W41882                                                         
012501     SKIP2                                                                
012502     MOVE 'S' TO POSTSUM-OPKOD                                            
012510     CALL POSTSUM USING POSTSUM-PARM                                      
012600     .                                                                    
012701     EJECT                                                                
012702 S01-LAES-W41880  SECTION.                                                
012703     READ W41880 INTO IN-AREA                                             
012704     AT END                                                               
012706        SET END-OF-W41880 TO TRUE                                         
012707                                                                          
012708     NOT AT END                                                           
012709        MOVE 'W41880' TO POSTSUM-FDNAMN                                   
012710        MOVE 'W41882D1' TO POSTSUM-DDNAMN2                                
012712        CALL POSTSUM USING POSTSUM-PARM                                   
012713     END-READ                                                             
012720     .                                                                    
012801     EJECT                                                                
012802 S11-SKRIV-W41882 SECTION.                                                
012803                                                                          
012804     WRITE UT-POST FROM UT-AREA                                           
012805                                                                          
012807     MOVE 'W41882' TO POSTSUM-FDNAMN                                      
012808     MOVE 'W41882D2' TO POSTSUM-DDNAMN2                                   
012809     CALL POSTSUM USING POSTSUM-PARM                                      
012810     .                                                                    
013000     EJECT                                                                
013100 S31-SORT-RELEASE  SECTION.                                               
013200                                                                          
013300     RELEASE SORT-POST FROM SORTWS-AREA                                   
013400     .                                                                    
013500     EJECT                                                                
013600 S32-SORT-RETURN  SECTION.                                                
013700                                                                          
013800     RETURN SORTFIL INTO SORTWS-AREA                                      
013900     AT END                                                               
014000         SET END-OF-SORTFIL TO TRUE                                       
014100     .                                                                    
014200     EJECT                                                                
014300 S99-ABEND SECTION.                                                       
014400                                                                          
014501     SKIP2                                                                
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
014700     .                                                                    
