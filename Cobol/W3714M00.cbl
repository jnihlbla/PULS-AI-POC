000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3714M00.                                                
000400 AUTHOR.         INGVAR SKJELBRED.                                        
000500 DATE-WRITTEN.   97/08/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000810*                                                                         
000900*    FUNKTION:                                                            
001000*        SLÅR IHOP POSTER MED SAMMA DC, FUNKTIONSGRUPP, DISTRIKT          
001100*        OCH ARTIKELNUMMER.                                               
001210*                                                                         
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
002402*          --- BYTES UPPFÖLJNINGSUNDERLAG                                 
002403     SELECT W3714K                     ASSIGN TO W3714MD1.                
002404     SKIP2                                                                
002405*          --- UT FIL MED BYTESUNDERLAG TILL BORN                         
002406     SELECT W3714M                     ASSIGN TO W3714MD2.                
002407                                                                          
002408     SKIP2                                                                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W3714K                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W3714J  -PRE IN-  -L.                                          
003007                                                                          
003008 FD  W3714M                                                               
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003012*01  POST -COPY W3714J -PRE UT-    -L.                                    
003013                                                                          
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W3714M00'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W3714K-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W3714K                       VALUE 'J'.                   
003900     EJECT                                                                
003910                                                                          
003980                                                                          
003981 77  PRAVCOST-HITTAD             PIC X       VALUE 'N'.                   
003982 77  BEART-HITTAD                PIC X       VALUE 'N'.                   
003983                                                                          
003990 01  SPAR-IDARTNR                PIC S9(9)   COMP-3 VALUE ZERO.           
003991 01  SPAR-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
003993 01  SPAR-IDFKNGRP               PIC S9(5)   COMP-3 VALUE ZERO.           
003994 01  SPAR-IDDC                   PIC X(2)           VALUE SPACE.          
003995 01  SPAR-BEART                  PIC X(25)          VALUE SPACE.          
003996                                                                          
004002 01  W-SUMMA                     PIC S9(7)   COMP-3 VALUE ZERO.           
004012                                                                          
004020 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005000     SKIP2                                                                
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006101     EJECT                                                                
006110*01  -COPY WDATAREA                                                       
006201     EJECT                                                                
006202 01  IN-AREA-START               PIC X(24)   VALUE                        
006203                                 'IN-AREA-START  '.                       
006204     SKIP2                                                                
006205                                                                          
006206*01  AREA -COPY W3714J     -PRE IN-                                       
006207     EJECT                                                                
006208                                                                          
006220                                                                          
006221 01  UT-AREA-START               PIC X(24)   VALUE                        
006222                                 'UT-AREA-START  '.                       
006223     SKIP2                                                                
006224                                                                          
006230*01  AREA -COPY W3714J     -PRE UT-                                       
006300     EJECT                                                                
006310                                                                          
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-LAES-W3714K                                              
007011     IF NOT END-OF-W3714K                                                 
007020       PERFORM S04-SPARA-UNDAN                                            
007030     END-IF                                                               
007100     PERFORM UNTIL END-OF-W3714K                                          
007110       PERFORM B-BEHANDLA                                                 
007810       PERFORM S01-LAES-W3714K                                            
007900     END-PERFORM                                                          
008000                                                                          
008010     PERFORM BD-SKRIV-SISTA-POSTEN                                        
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008910     OPEN INPUT  W3714K                                                   
009001                                                                          
009010     OPEN OUTPUT W3714M                                                   
009100     SKIP2                                                                
009200     ACCEPT DAGENS-DATUM  FROM DATE                                       
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009320     PERFORM BB-NOLLSTALL                                                 
009400     .                                                                    
009500     EJECT                                                                
009510 B-BEHANDLA SECTION.                                                      
009511                                                                          
009520                                                                          
009530     IF SPAR-IDARTNR NOT = IN-IDARTNR                                     
009533        PERFORM BC-FLYTTA-TILL-UTFIL                                      
009550        PERFORM S11-SKRIV-W3714M                                          
009551        PERFORM S04-SPARA-UNDAN                                           
009552        PERFORM BB-NOLLSTALL                                              
009553        PERFORM BA-BERAKNA                                                
009560     ELSE                                                                 
009571        IF SPAR-IDDISTR  NOT = IN-IDDISTR                                 
009574           PERFORM BC-FLYTTA-TILL-UTFIL                                   
009590           PERFORM S11-SKRIV-W3714M                                       
009591           PERFORM S04-SPARA-UNDAN                                        
009592           PERFORM BB-NOLLSTALL                                           
009593           PERFORM BA-BERAKNA                                             
009594        ELSE                                                              
009604           IF SPAR-IDFKNGRP NOT = IN-IDFKNGRP                             
009607              PERFORM BC-FLYTTA-TILL-UTFIL                                
009608              PERFORM S11-SKRIV-W3714M                                    
009609              PERFORM S04-SPARA-UNDAN                                     
009610              PERFORM BB-NOLLSTALL                                        
009611              PERFORM BA-BERAKNA                                          
009612           ELSE                                                           
009614              PERFORM BA-BERAKNA                                          
009615           END-IF                                                         
009617        END-IF                                                            
009618     END-IF                                                               
009619                                                                          
009620     .                                                                    
009621     EJECT                                                                
009622 BA-BERAKNA SECTION.                                                      
009626                                                                          
009627     COMPUTE W-SUMMA  = W-SUMMA + IN-KVRETUR                              
009631                                                                          
009668     .                                                                    
009669     EJECT                                                                
009670 BB-NOLLSTALL SECTION.                                                    
009672                                                                          
009673     MOVE ZERO            TO W-SUMMA                                      
009679                                                                          
009680     .                                                                    
009681     EJECT                                                                
009682 BC-FLYTTA-TILL-UTFIL SECTION.                                            
009683                                                                          
009684     MOVE SPAR-IDARTNR        TO UT-IDARTNR                               
009685     MOVE SPAR-IDDISTR        TO UT-IDDISTR                               
009687     MOVE SPAR-IDDC           TO UT-IDDC                                  
009688     MOVE SPAR-IDFKNGRP       TO UT-IDFKNGRP                              
009689     MOVE SPAR-BEART          TO UT-BEART                                 
009690     MOVE W-SUMMA             TO UT-KVRETUR                               
009696                                                                          
009697     .                                                                    
009698     EJECT                                                                
009699 BD-SKRIV-SISTA-POSTEN SECTION.                                           
009700                                                                          
009702     PERFORM BC-FLYTTA-TILL-UTFIL                                         
009703     PERFORM S11-SKRIV-W3714M                                             
009704                                                                          
009705     .                                                                    
009706     EJECT                                                                
009707 Z-FINIT SECTION.                                                         
009708     CLOSE W3714K                                                         
009709           W3714M                                                         
009712     SKIP2                                                                
009713     MOVE 'S' TO POSTSUM-OPKOD                                            
009714     CALL POSTSUM USING POSTSUM-PARM                                      
009715     .                                                                    
009720     EJECT                                                                
009750 S01-LAES-W3714K  SECTION.                                                
009751                                                                          
009752     READ W3714K INTO IN-AREA                                             
009753     AT END                                                               
009754        MOVE HIGH-VALUE TO IN-AREA                                        
009755        SET END-OF-W3714K TO TRUE                                         
009756                                                                          
009757     NOT AT END                                                           
009758        MOVE 'W3714K' TO POSTSUM-FDNAMN                                   
009759        MOVE 'W3714MD1' TO POSTSUM-DDNAMN2                                
009760        MOVE 'IN  '       TO POSTSUM-TRANSTYP                             
009762        CALL POSTSUM USING POSTSUM-PARM                                   
009763     END-READ                                                             
009764     .                                                                    
009765     EJECT                                                                
009799 S04-SPARA-UNDAN SECTION.                                                 
009801                                                                          
009804     MOVE IN-IDDISTR      TO SPAR-IDDISTR                                 
009806     MOVE IN-IDDC         TO SPAR-IDDC                                    
009807     MOVE IN-IDARTNR      TO SPAR-IDARTNR                                 
009808     MOVE IN-IDFKNGRP     TO SPAR-IDFKNGRP                                
009809     MOVE IN-BEART        TO SPAR-BEART                                   
009812                                                                          
009813     .                                                                    
009814     EJECT                                                                
009877 S11-SKRIV-W3714M SECTION.                                                
009878                                                                          
009879     WRITE UT-POST FROM UT-AREA                                           
009880                                                                          
009881     MOVE 'W3714M' TO POSTSUM-FDNAMN                                      
009882     MOVE 'W3714MD2' TO POSTSUM-DDNAMN2                                   
009883     MOVE 'UT  '       TO POSTSUM-TRANSTYP                                
009884     CALL POSTSUM USING POSTSUM-PARM                                      
009885     .                                                                    
009886     EJECT                                                                
009887 S99-ABEND SECTION.                                                       
009888                                                                          
009889     SKIP2                                                                
009890     MOVE 'S' TO POSTSUM-OPKOD                                            
009891     CALL POSTSUM USING POSTSUM-PARM                                      
009892     CALL ABEND USING RKOD-ABEND                                          
009893                                                                          
009894     .                                                                    
009900     EJECT                                                                
