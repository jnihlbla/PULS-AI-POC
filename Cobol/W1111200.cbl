000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W1111200.                                                
000500*AUTHOR.         STEFAN KIHLBERG.                                         
000600*DATE-WRITTEN.   93/07/30.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001010*                                                                         
001020*        GEMENSAMMA ARTIKLAR                                              
001030*                                                                         
001100*        MATCHAR EN FIL MED EGNA BOLAGETS GÄLLANDE ARTIKLAR               
001200*        MED EN FIL MED ANDRA BOLAGETS GÄLLANDE ARTIKLAR.                 
001300*                                                                         
001400*        SKRIVER FIL MED  ARTIKLAR DÄR FLGEMART SKALL                     
001410*        UPPDATERAS.                                                      
001420*                                                                         
001500*        NÄR ARTIKELN FINNS PÅ BÅDA BOLAGEN OCH EGEN FLGEMART             
001600*        INTE = JA          ===> BLIR FLGEMART PÅ UTFIL = JA              
001700*        NÄR ARTIKELN SAKNAS PÅ ANDRA BOLAGET OCH EGEN FLGEMART           
001800*        INTE = NEJ         ===> BLIR FLGEMART PÅ UTFIL = NEJ             
001900*                                                                         
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- GÄLLANDE ARTIKLAR EGNA BOLAGET                             
003203     SELECT W11111                     ASSIGN TO W11112D1.                
003204     SKIP2                                                                
003205*          --- GÄLLANDE ARTIKLAR ANDRA BOLAGET                            
003206     SELECT R11111                     ASSIGN TO W11112D2.                
003207     SKIP2                                                                
003208*          --- ARTIKLAR DÄR FLGEMART SKALL UPPDATERAS                     
003210     SELECT W11113                     ASSIGN TO W11112D3.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W11111                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805     SKIP2                                                                
003806*01  -COPY W11111      -L.                                                
003807     SKIP3                                                                
003808 FD  R11111                                                               
003809     RECORDING       F                                                    
003810     BLOCK CONTAINS  0.                                                   
003811     SKIP2                                                                
003812*01  -COPY W11111      -L.                                                
003813     SKIP3                                                                
003814 FD  W11113                                                               
003815     RECORDING       F                                                    
003816     BLOCK CONTAINS  0.                                                   
003817     SKIP2                                                                
003820*01  POST -COPY W11113 -PRE  W11113-  -L.                                 
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W1111200'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004601                                                                          
004602 77  W11111-EOF-SW               PIC X       VALUE 'N'.                   
004603     88  END-OF-W11111                       VALUE 'J'.                   
004604                                                                          
004605 77  R11111-EOF-SW               PIC X       VALUE 'N'.                   
004610     88  END-OF-R11111                       VALUE 'J'.                   
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
006100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006300     SKIP2                                                                
006400 01  FELTEXT.                                                             
006500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006701     EJECT                                                                
006702*    --- PARAMETRAR TILL POSTSUM                                          
006703*                                                                         
006710*01  -COPY W0005   -PRE  POSTSUM-                                         
006901     EJECT                                                                
006902 01  W11111-AREA-START           PIC X(24)   VALUE                        
006903                                 'W11111-AREA-START  '.                   
006904     SKIP2                                                                
006905                                                                          
006906*01  AREA -COPY W11111     -PRE W11111-                                   
006907     EJECT                                                                
006908 01  R11111-AREA-START           PIC X(24)   VALUE                        
006909                                 'R11111-AREA-START  '.                   
006910     SKIP2                                                                
006911                                                                          
006912*01  AREA -COPY W11111     -PRE R11111-                                   
006913     EJECT                                                                
006914 01  W11113-AREA-START           PIC X(24)   VALUE                        
006915                                 'W11113-AREA-START  '.                   
006916     SKIP2                                                                
006917                                                                          
006920*01  AREA -COPY W11113     -PRE W11113-                                   
007000     EJECT                                                                
007100 PROCEDURE DIVISION.                                                      
007300     SKIP2                                                                
007400                                                                          
007500     PERFORM A-INIT                                                       
007601     PERFORM S01-LAES-W11111                                              
007610     PERFORM S02-LAES-R11111                                              
007700     PERFORM UNTIL END-OF-W11111                                          
007800        IF W11111-IDARTNR = R11111-IDARTNR                                
007900           PERFORM B-ARTIKEL-PA-BADA                                      
007910           PERFORM S01-LAES-W11111                                        
007920           PERFORM S02-LAES-R11111                                        
008000        ELSE                                                              
008100           IF W11111-IDARTNR < R11111-IDARTNR                             
008200              PERFORM C-ARTIKEL-SAKNAS-PA-ANDRA                           
008210              PERFORM S01-LAES-W11111                                     
008300           ELSE                                                           
008310              PERFORM S02-LAES-R11111                                     
008400           END-IF                                                         
008401        END-IF                                                            
008500     END-PERFORM                                                          
008800     PERFORM Z-FINIT                                                      
008900                                                                          
009000     MOVE ZERO TO RETURN-CODE                                             
009100     GOBACK                                                               
009200     .                                                                    
009300     EJECT                                                                
009400 A-INIT SECTION.                                                          
009501                                                                          
009502     OPEN INPUT  W11111                                                   
009510                 R11111                                                   
009601                                                                          
009610     OPEN OUTPUT W11113                                                   
009700     SKIP2                                                                
009800     ACCEPT DAGENS-DATUM  FROM DATE                                       
009910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010000     .                                                                    
010100     EJECT                                                                
010101                                                                          
010102                                                                          
010110 B-ARTIKEL-PA-BADA SECTION.                                               
010120                                                                          
010130     IF W11111-FLGEMART NOT = JA                                          
010151        MOVE W11111-IDARTNR     TO W11113-IDARTNR                         
010152        MOVE JA                 TO W11113-FLGEMART                        
010153        PERFORM S11-SKRIV-W11113                                          
010160     END-IF                                                               
010170     .                                                                    
010180     EJECT                                                                
010190                                                                          
010199                                                                          
010200 C-ARTIKEL-SAKNAS-PA-ANDRA SECTION.                                       
010210                                                                          
010220     IF W11111-FLGEMART NOT = NEJ                                         
010231        MOVE W11111-IDARTNR     TO W11113-IDARTNR                         
010232        MOVE NEJ                TO W11113-FLGEMART                        
010240        PERFORM S11-SKRIV-W11113                                          
010250     END-IF                                                               
010260     .                                                                    
010270     EJECT                                                                
010280                                                                          
010298                                                                          
010300 Z-FINIT SECTION.                                                         
010301     CLOSE W11111                                                         
010302           R11111                                                         
010310           W11113                                                         
010401     SKIP2                                                                
010402     MOVE 'S' TO POSTSUM-OPKOD                                            
010410     CALL POSTSUM USING POSTSUM-PARM                                      
010500     .                                                                    
010601     EJECT                                                                
010602 S01-LAES-W11111  SECTION.                                                
010603     SKIP2                                                                
010604     READ W11111 INTO W11111-AREA                                         
010605     AT END                                                               
010607        SET END-OF-W11111 TO TRUE                                         
010608                                                                          
010609     NOT AT END                                                           
010610        MOVE 'W11111' TO POSTSUM-FDNAMN                                   
010611        MOVE 'W11112D1' TO POSTSUM-DDNAMN2                                
010613        CALL POSTSUM USING POSTSUM-PARM                                   
010614     END-READ                                                             
010615     .                                                                    
010616     EJECT                                                                
010617 S02-LAES-R11111  SECTION.                                                
010618     SKIP2                                                                
010619     READ R11111 INTO R11111-AREA                                         
010620     AT END                                                               
010621        MOVE +999999999 TO R11111-IDARTNR                                 
010622        SET END-OF-R11111 TO TRUE                                         
010623                                                                          
010624     NOT AT END                                                           
010625        MOVE 'R11111' TO POSTSUM-FDNAMN                                   
010626        MOVE 'W11112D2' TO POSTSUM-DDNAMN2                                
010628        CALL POSTSUM USING POSTSUM-PARM                                   
010629     END-READ                                                             
010630     .                                                                    
010701     EJECT                                                                
010702 S11-SKRIV-W11113 SECTION.                                                
010703     SKIP2                                                                
010704     WRITE W11113-POST FROM W11113-AREA                                   
010705                                                                          
010707     MOVE 'W11113' TO POSTSUM-FDNAMN                                      
010708     MOVE 'W11112D3' TO POSTSUM-DDNAMN2                                   
010709     CALL POSTSUM USING POSTSUM-PARM                                      
010710     .                                                                    
010900     EJECT                                                                
