000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3713800.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   95/05/02.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKAPAR TULLINFORMATION.                                          
001000*                                                                         
001120*                                                                         
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
002400*          --- HISTORIKFIL                                                
002500     SELECT W37138                     ASSIGN TO W37138D1.                
002600     SKIP2                                                                
002700*          --- VALT DISTRIKT FRANKRIKE                                    
002800     SELECT W37139                     ASSIGN TO W37138D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W37138                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W37138      -L.                                                
003900     SKIP3                                                                
004000 FD  W37139                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W37138 -PRE  TULL-  -L.                                   
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004701                                                                          
004710*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                       PIC X(8)    VALUE 'W3713800'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  W37138-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W37138                       VALUE 'J'.                   
005400     EJECT                                                                
005410 01  DAGENS-DATUM                PIC 9(6) VALUE ZERO.                     
005420 01  RED-DATUM                   REDEFINES   DAGENS-DATUM.                
005430     03   DAGENS-DATUM-AR        PIC 9(2).                                
005440     03   DAGENS-DATUM-MANAD     PIC 9(2).                                
005450     03   DAGENS-DATUM-DAG       PIC 9(2).                                
006000*                                                                         
006070 01  SPLIT-DAGENS-AR             PIC 9(2).                                
006080 01  RED-SPLIT                   REDEFINES   SPLIT-DAGENS-AR.             
006090     03   FILLER                 PIC 9(1).                                
006091     03   NUV-AR                 PIC 9(1).                                
006092                                                                          
006100 01  WS-DATUM                     PIC 9(6)    VALUE ZERO.                 
006200 01  FILLER REDEFINES WS-DATUM.                                           
006300     03  WS-DATUM-AAMM            PIC 9(4).                               
006400     03  WS-DATUM-DAG             PIC 9(2).                               
006500*                                                                         
006600 01  WS-HIST-DATUM                PIC 9(6)    VALUE ZERO.                 
006700 01  FILLER REDEFINES WS-HIST-DATUM.                                      
006800     03  WS-HIST-DATUM-AAMM       PIC 9(4).                               
006900     03  WS-HIST-DATUM-DAG        PIC 9(2).                               
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007220     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     SKIP2                                                                
007510     SKIP3                                                                
007520*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
007530                                                                          
007540 01  FILLER                   PIC X(16) VALUE 'DATKORT'.                  
007550 01  DATUMKORT-ID             PIC X(6)  VALUE 'WDATUM'.                   
007560*01  -COPY WDATKORT                                                       
007570     EJECT                                                                
007600*    --- PARAMETRAR TILL ABEND                                            
007700                                                                          
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900     SKIP3                                                                
009000 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
009100*01  FILLER  -COPY WWDIST01   -RED TEST-IDDISTR.                          
009200     EJECT                                                                
009300 01  HIST-AREA-START             PIC X(24)   VALUE                        
009400                                 'HIST-AREA-START  '.                     
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY W37138     -PRE HIST-                                     
009800     EJECT                                                                
009900 01  TULL-AREA-START             PIC X(24)   VALUE                        
010000                                 'TULL-AREA-START  '.                     
010100     SKIP2                                                                
010200                                                                          
010300*01  AREA -COPY W37138     -PRE TULL-                                     
010400     EJECT                                                                
010500 PROCEDURE DIVISION.                                                      
010600     SKIP2                                                                
010700                                                                          
010800     PERFORM A-INIT                                                       
010900     PERFORM S01-LAES-W37138                                              
011000     PERFORM UNTIL END-OF-W37138                                          
011100       MOVE HIST-IDDISTR TO TEST-IDDISTR                                  
011200       IF  DIST01-FRANCE-TULL                                             
011201       OR  DIST01-ITALIEN-TULL                                            
011202       OR  DIST01-ENGLAND-TULL                                            
011203       OR  DIST01-SPANIEN-TULL                                            
011204       OR  DIST01-AUSTRIA-TULL                                            
011210         MOVE HIST-TIREGDAT-GODK TO WS-HIST-DATUM                         
011310         IF WS-DATUM-AAMM = WS-HIST-DATUM-AAMM                            
011400           PERFORM B-SKRIVFIL                                             
011500         END-IF                                                           
011600       END-IF                                                             
011700       PERFORM S01-LAES-W37138                                            
011800     END-PERFORM                                                          
011900                                                                          
012000                                                                          
012100     PERFORM Z-FINIT                                                      
012200                                                                          
012300     MOVE ZERO TO RETURN-CODE                                             
012400     GOBACK                                                               
012500     .                                                                    
012600     EJECT                                                                
012700 A-INIT SECTION.                                                          
012800                                                                          
012900     OPEN INPUT  W37138                                                   
013000                                                                          
013100     OPEN OUTPUT W37139                                                   
013200     SKIP2                                                                
013220                                                                          
013230     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
013240     MOVE D-AAR      TO DAGENS-DATUM-AR                                   
013250     MOVE D-AAR      TO SPLIT-DAGENS-AR                                   
013260     MOVE D-MAANAD   TO DAGENS-DATUM-MANAD                                
013270     MOVE D-DAG      TO DAGENS-DATUM-DAG                                  
013280                                                                          
013291*    MOVE NUV-AR     TO WS-NUV-AR                                         
014000     MOVE DAGENS-DATUM TO WS-DATUM                                        
014100                                                                          
014200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014300     .                                                                    
014400     EJECT                                                                
014410 B-SKRIVFIL SECTION.                                                      
014411     SKIP2                                                                
014412     MOVE HIST-W37138 TO TULL-W37138                                      
014413     PERFORM S11-SKRIV-W37139                                             
014466     .                                                                    
014470     EJECT                                                                
014500 Z-FINIT SECTION.                                                         
014600     CLOSE W37138                                                         
014700           W37139                                                         
014800     SKIP2                                                                
014900     MOVE 'S' TO POSTSUM-OPKOD                                            
015000     CALL POSTSUM USING POSTSUM-PARM                                      
015100     .                                                                    
015200     EJECT                                                                
015300 S01-LAES-W37138  SECTION.                                                
015400     READ W37138 INTO HIST-AREA                                           
015500     AT END                                                               
015700        SET END-OF-W37138 TO TRUE                                         
015800                                                                          
015900     NOT AT END                                                           
016000        MOVE 'W37138'   TO POSTSUM-FDNAMN                                 
016100        MOVE 'W37138D1' TO POSTSUM-DDNAMN2                                
016200        MOVE 'HIST'     TO POSTSUM-TRANSTYP                               
016300        CALL POSTSUM USING POSTSUM-PARM                                   
016400     END-READ                                                             
016500     .                                                                    
016600     EJECT                                                                
016700 S11-SKRIV-W37139 SECTION.                                                
016800                                                                          
016900     WRITE TULL-POST FROM TULL-AREA                                       
017000                                                                          
017100     MOVE 'TULL'     TO POSTSUM-TRANSTYP                                  
017200     MOVE 'W37139'   TO POSTSUM-FDNAMN                                    
017300     MOVE 'W37138D2' TO POSTSUM-DDNAMN2                                   
017400     CALL POSTSUM USING POSTSUM-PARM                                      
017500     .                                                                    
