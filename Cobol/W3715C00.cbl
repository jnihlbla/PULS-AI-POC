000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3715C00.                                                
000300 AUTHOR.         INGVAR SKJELBRED.                                        
000400 DATE-WRITTEN.   98/03/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SLÅR IHOP UPPFÖLJNINGSFILEN FRÅN FÖRSÄLJNINGSTATISTIKEN          
000910*        OCH BYTESUPPFÖLJNINGSFILEN OCH SKAPAR REGISTER POSTER            
000920*        SOM LIGGER I TVÅ ÅR                                              
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
002302*          --- FÖRSÄLJNINGSSTATISTIK FÖR UPPFÖLJNING                      
002303     SELECT W3715A                     ASSIGN TO W3715CD1.                
002304     SKIP2                                                                
002305*          --- BYTESUPPFÖLJNINGSFIL MED R56:OR                            
002306     SELECT W3715B                     ASSIGN TO W3715CD2.                
002307*          --- ARTIKELREGISTRET PÅ FIL                                    
002308     SELECT W01160                     ASSIGN TO W3715CD3.                
002309     SKIP2                                                                
002310*          --- BENÄMNINGSREGISTRET PÅ FIL                                 
002311     SELECT W01174                     ASSIGN TO W3715CD4.                
002312     SKIP2                                                                
002313     SKIP2                                                                
002314*          --- INKOMMANDE REGISTER 2 ÅR RULLANDE                          
002315     SELECT REG-IN                     ASSIGN TO W3715CD5.                
002316     SKIP2                                                                
002317*          --- UTGÅENDE REGISTER RULLANDE 2 ÅR                            
002320     SELECT REG-UT                     ASSIGN TO W3715CD6.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002901     SKIP3                                                                
002902 FD  W3715A                                                               
002903     RECORDING       F                                                    
002904     BLOCK CONTAINS  0.                                                   
002905                                                                          
002906*01  -COPY W3715A      -L.                                                
002907     SKIP3                                                                
002908 FD  W3715B                                                               
002909     RECORDING       F                                                    
002910     BLOCK CONTAINS  0.                                                   
002911                                                                          
002912*01  -COPY W3715B      -L.                                                
002913     SKIP3                                                                
002914 FD  REG-IN                                                               
002915     RECORDING       F                                                    
002916     BLOCK CONTAINS  0.                                                   
002917                                                                          
002918*01  FILLER -COPY W3715C     -L.                                          
002919                                                                          
002920 FD  W01160                                                               
002921     RECORDING       F                                                    
002922     BLOCK CONTAINS  0.                                                   
002923                                                                          
002924*01  -COPY W01160      -L.                                                
002925     SKIP3                                                                
002926 FD  W01174                                                               
002927     RECORDING       F                                                    
002928     BLOCK CONTAINS  0.                                                   
002929                                                                          
002930*01  -COPY W01174      -L.                                                
002931     SKIP3                                                                
002932     SKIP3                                                                
002933 FD  REG-UT                                                               
002934     RECORDING       F                                                    
002935     BLOCK CONTAINS  0.                                                   
002936                                                                          
002940*01  POST -COPY W3715C -PRE  REG-UT-  -L.                                 
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003201*    -- CHECKED BY WY2000                                                 
003210                                                                          
003300 77  IDPGM                       PIC X(8)    VALUE 'W3715C00'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003701                                                                          
003702 77  W3715A-EOF-SW               PIC X       VALUE 'N'.                   
003703     88  END-OF-W3715A                       VALUE 'J'.                   
003704                                                                          
003705 77  W3715B-EOF-SW               PIC X       VALUE 'N'.                   
003706     88  END-OF-W3715B                       VALUE 'J'.                   
003708                                                                          
003709 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
003710     88  END-OF-W01160                       VALUE 'J'.                   
003711     EJECT                                                                
003712                                                                          
003713                                                                          
003714 77  W01174-EOF-SW               PIC X       VALUE 'N'.                   
003715     88  END-OF-W01174                       VALUE 'J'.                   
003716     EJECT                                                                
003717                                                                          
003718 77  REG-IN-EOF-SW               PIC X       VALUE 'N'.                   
003720     88  END-OF-REG-IN                       VALUE 'J'.                   
003721                                                                          
003723 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
003724 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
003725 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
003726 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
003727 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
003728 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
003729 01  FILLER                  PIC X(16)   VALUE 'WS-SUB-SEKTION'.          
003730 01  WS-SUB-SEKTION              PIC X(30)   VALUE SPACE.                 
003731                                                                          
003732 01  DAGENS-SSAAVV               PIC 9(7).                                
003733 01  FILLER REDEFINES DAGENS-SSAAVV.                                      
003734     03  FILLER                  PIC 9.                                   
003735     03  DAGENS-SS-VV            PIC 9(2).                                
003736     03  DAGENS-AA-VV            PIC 9(2).                                
003737     03  DAGENS-VV               PIC 9(2).                                
003738                                                                          
003739 01  SPAR-IDARTNR                PIC S9(9) VALUE ZERO COMP-3.             
003740 01  SPAR-IDARTNR-OBJ            PIC S9(9) VALUE ZERO COMP-3.             
003741 01  SPAR-TIFSGVV                PIC 9(7) VALUE ZERO COMP-3.              
003742 01  SPAR-IDDISTR                PIC S9(5) VALUE ZERO COMP-3.             
003743 01  SPAR-IDBYTRAP               PIC S9(7) VALUE ZERO COMP-3.             
003750                                                                          
003800     EJECT                                                                
003900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004000 01  FILLER REDEFINES DAGENS-DATUM.                                       
004100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004400     EJECT                                                                
004410*      --- VALID IDDC CODES                                               
004420*                                                                         
004430*01    -COPY WWDCKONS                                                     
004440       EJECT                                                              
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004801     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004820     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
004830     03  W510MARK                PIC X(8)    VALUE 'W510MARK'.            
004900     SKIP2                                                                
005000*    --- PARAMETRAR TILL ABEND                                            
005100                                                                          
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005900                                                                          
005901*    --- PARAMETRAR TILL DATKORT                                          
005902*                                                                         
005903 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W3715C'.              
005904     SKIP2                                                                
005905 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
005906     SKIP2                                                                
005907*01  -COPY WDATKORT                                                       
005908                                                                          
005909 01  FILLER      PIC X(8)  VALUE 'W510MARK'.                              
005910*01  -COPY W510MARK                                                       
005912     EJECT                                                                
005913*    --- PARAMETRAR TILL POSTSUM                                          
005914*                                                                         
005920*01  -COPY W0005   -PRE  POSTSUM-                                         
006001     EJECT                                                                
006102 01  IN1-AREA-START              PIC X(24)   VALUE                        
006103                                 'IN1-AREA-START  '.                      
006104     SKIP2                                                                
006105                                                                          
006106*01  AREA -COPY W3715A     -PRE IN1-                                      
006107     EJECT                                                                
006108 01  IN2-AREA-START              PIC X(24)   VALUE                        
006109                                 'IN2-AREA-START  '.                      
006110     SKIP2                                                                
006111                                                                          
006112*01  AREA -COPY W3715B     -PRE IN2-                                      
006113     EJECT                                                                
006114                                                                          
006115*01  AREA -COPY W01160     -PRE IN3-                                      
006116     EJECT                                                                
006117 01  UT-AREA-START               PIC X(24)   VALUE                        
006118                                 'UT-AREA-START  '.                       
006119     SKIP2                                                                
006120                                                                          
006121 01  IN3-AREA-START              PIC X(24)   VALUE                        
006122                                 'IN3-AREA-START  '.                      
006123     SKIP2                                                                
006124                                                                          
006125*01  AREA -COPY W01174     -PRE IN4-                                      
006126     EJECT                                                                
006127 01  INREG-AREA-START            PIC X(24)   VALUE                        
006128                                 'INREG-AREA-START  '.                    
006129     SKIP2                                                                
006130                                                                          
006131******************************************************************        
006132*         REG-IN-AREA                                            *        
006133******************************************************************        
006134 01  FILLER                      PIC X(24)   VALUE                        
006135                                 'REG-IN-AREA'.                           
006136 01  REG-IN-AREA.                                                         
006137*    03  FILLER -COPY W3715C     -PRE REG-IN-.                            
006138     EJECT                                                                
006139******************************************************************        
006140*         REG-UT-AREA                                            *        
006141******************************************************************        
006142 01  FILLER                      PIC X(24)   VALUE                        
006143                                 'REG-UT-AREA'.                           
006144 01  REG-UT-AREA.                                                         
006145*    03  FILLER -COPY W3715C     -PRE REG-UT-.                            
006146     EJECT                                                                
006150                                                                          
006300 PROCEDURE DIVISION.                                                      
006400 MAIN SECTION.                                                            
006700                                                                          
006800     PERFORM A-INIT                                                       
006900     PERFORM S03-LAES-W01160                                              
006910     PERFORM S04-LAES-W01174                                              
006920     PERFORM S05-LAS-REG-IN                                               
006922     PERFORM UNTIL END-OF-REG-IN                                          
006923**********************************************                            
006924*** DAGENS-SSAAVV   INNEBÄR ETT TVÅ ÅRS    ***                            
006925*** TILLÄGG PÅ DAGENSDATUM (DAGENS ÅR)     ***                            
006926**********************************************                            
006927       IF DAGENS-SSAAVV = REG-IN-TIFSGVV                                  
006928       OR DAGENS-SSAAVV > REG-IN-TIFSGVV                                  
006929          PERFORM B-FLYTTA-TILL-UTREG                                     
006930          PERFORM S11-SKRIV-REG-UT                                        
006931       END-IF                                                             
006932       PERFORM S05-LAS-REG-IN                                             
006933     END-PERFORM                                                          
006940                                                                          
006950     PERFORM S01-LAES-W3715A                                              
006960     PERFORM S02-LAES-W3715B                                              
006970     PERFORM UNTIL END-OF-W3715A                                          
006980              AND  END-OF-W3715B                                          
007101       PERFORM C-KONTRL-IN-FILER                                          
007800     END-PERFORM                                                          
007900                                                                          
008000                                                                          
008100     PERFORM Z-FINIT                                                      
008200                                                                          
008300     MOVE ZERO TO RETURN-CODE                                             
008400     GOBACK                                                               
008500     .                                                                    
008600     EJECT                                                                
008700 A-INIT SECTION.                                                          
008800     MOVE 'A-INIT '      TO WS-SEKTION                                    
008801                                                                          
008802     OPEN INPUT  W3715A                                                   
008803                 W3715B                                                   
008804                 W01160                                                   
008805                 W01174                                                   
008806                 REG-IN                                                   
008901                                                                          
008910     OPEN OUTPUT REG-UT                                                   
008912                                                                          
008920     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
008921****************************************                                  
008922*** ÖKAR INNEVARANDE ÅR MED TVÅ ÅR   ***                                  
008923*** ANVÄNDS FÖR RENSNING AV REGISTER ***                                  
008924****************************************                                  
008930     ADD  2           TO D-AAR                                            
008931     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
008940                         DAGENS-AA-VV                                     
008950     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
008960     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
008970     MOVE D-VECKA     TO DAGENS-VV                                        
008980                                                                          
008990     IF D-AAR > 60                                                        
008991        MOVE 19      TO DAGENS-SS-VV                                      
008992     ELSE                                                                 
008993        MOVE 20      TO DAGENS-SS-VV                                      
008994     END-IF                                                               
008995                                                                          
009000     SKIP2                                                                
009210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009220     DISPLAY 'DAGENS-SSAAVV ' DAGENS-SSAAVV                               
009300     .                                                                    
009400     EJECT                                                                
009401                                                                          
009410 B-FLYTTA-TILL-UTREG SECTION.                                             
009411     MOVE 'B-FLYTTA-TILL-UTREG' TO WS-SEKTION                             
009420                                                                          
009421     MOVE REG-IN-AREA TO REG-UT-AREA                                      
009422                                                                          
009423     .                                                                    
009430     EJECT                                                                
009440                                                                          
009478 C-KONTRL-IN-FILER SECTION.                                               
009479     MOVE 'C-KONTRL-IN-FILER' TO WS-SEKTION                               
009480                                                                          
009481     IF END-OF-W3715A                                                     
009482**************************************************                        
009483**** ENDAST BYTES RETURER ELLER SK R56 ***********                        
009484**************************************************                        
009485        PERFORM CD-SKAPA-REGISTER                                         
009486        PERFORM S11-SKRIV-REG-UT                                          
009487        PERFORM S02-LAES-W3715B                                           
009488     ELSE                                                                 
009489       IF END-OF-W3715B                                                   
009490**************************************************                        
009491**** ENDAST FÖRSÄLJNINGS STATISTIK ***************                        
009492**************************************************                        
009493          PERFORM CC-SKAPA-REGISTER                                       
009494          PERFORM S11-SKRIV-REG-UT                                        
009495          PERFORM S01-LAES-W3715A                                         
009496       ELSE                                                               
009497         IF  IN1-IDARTNR = IN2-IDARTNR                                    
009498         AND IN1-IDARTNR-OBJ = IN2-IDARTNR-OBJ                            
009499         AND IN1-TIFSGVV = IN2-TIFSGVV                                    
009500         AND IN1-IDDISTR = IN2-IDDISTR                                    
009504             PERFORM CA-SKAPA-REGISTER                                    
009505             MOVE ZERO         TO SPAR-IDBYTRAP                           
009506             MOVE IN2-IDBYTRAP TO SPAR-IDBYTRAP                           
009507             PERFORM UNTIL END-OF-W3715B                                  
009508                       OR IN1-IDARTNR < IN2-IDARTNR                       
009509                       OR IN1-IDARTNR-OBJ < IN2-IDARTNR-OBJ               
009510                       OR IN1-TIFSGVV < IN2-TIFSGVV                       
009511                       OR IN1-IDDISTR < IN2-IDDISTR                       
009512                       IF SPAR-IDBYTRAP NOT = IN2-IDBYTRAP                
009513                          PERFORM S11-SKRIV-REG-UT                        
009514                          MOVE IN2-IDBYTRAP TO SPAR-IDBYTRAP              
009515                          PERFORM CA-SKAPA-REGISTER                       
009516                       ELSE                                               
009517                          ADD IN2-KVRETUR-URSP                            
009518                                 TO REG-UT-KVRETUR-URSP                   
009519                          ADD IN2-KVRETUR-GODK                            
009520                                 TO REG-UT-KVRETUR-GODK                   
009521                       END-IF                                             
009522                       PERFORM S02-LAES-W3715B                            
009523             END-PERFORM                                                  
009524             PERFORM S11-SKRIV-REG-UT                                     
009525             PERFORM S01-LAES-W3715A                                      
009526         ELSE                                                             
009527           IF IN1-IDARTNR > IN2-IDARTNR                                   
009528           OR IN1-IDARTNR-OBJ > IN2-IDARTNR-OBJ                           
009529           OR IN1-TIFSGVV > IN2-TIFSGVV                                   
009530           OR IN1-IDDISTR > IN2-IDDISTR                                   
009535              PERFORM CD-SKAPA-REGISTER                                   
009536              PERFORM S11-SKRIV-REG-UT                                    
009537              PERFORM S02-LAES-W3715B                                     
009538           ELSE                                                           
009544              PERFORM CC-SKAPA-REGISTER                                   
009545              PERFORM S11-SKRIV-REG-UT                                    
009546              PERFORM S01-LAES-W3715A                                     
009547           END-IF                                                         
009548         END-IF                                                           
009549       END-IF                                                             
009550     END-IF                                                               
009551                                                                          
009552     .                                                                    
009553     EJECT                                                                
009554                                                                          
009555 CA-SKAPA-REGISTER SECTION.                                               
009556     MOVE 'CA-SKAPA-REGISTER' TO WS-SEKTION                               
009557                                                                          
009558     MOVE IN1-IDARTNR      TO REG-UT-IDARTNR                              
009559     MOVE IN1-IDARTNR-OBJ  TO REG-UT-IDARTNR-OBJ                          
009560     MOVE IN1-KDPRODSL     TO REG-UT-KDPRODSL                             
009561     MOVE IN1-IDFKNGRP     TO REG-UT-IDFKNGRP                             
009562     MOVE IN1-BEART        TO REG-UT-BEART                                
009563     MOVE IN1-IDDISTR      TO REG-UT-IDDISTR                              
009564     PERFORM S12-ANROPA-W510MARK                                          
009565     MOVE IN1-TIFSGVV      TO REG-UT-TIFSGVV                              
009566     MOVE IN1-SUARTFSG     TO REG-UT-SUARTFSG                             
009567     MOVE IN1-SULEVANT     TO REG-UT-SULEVANT                             
009568     MOVE IN2-KVRETUR-URSP TO REG-UT-KVRETUR-URSP                         
009569     MOVE IN2-KVRETUR-GODK TO REG-UT-KVRETUR-GODK                         
009570     MOVE IN2-IDTABNR      TO REG-UT-IDTABNR                              
009571     MOVE IN2-IDBYTRAP     TO REG-UT-IDBYTRAP                             
009572     MOVE IN2-IDDC         TO REG-UT-IDDC                                 
009573                                                                          
009574     .                                                                    
009575     EJECT                                                                
009576                                                                          
009594 CC-SKAPA-REGISTER SECTION.                                               
009595     MOVE 'CC-SKAPA-REGISTER' TO WS-SEKTION                               
009596                                                                          
009597     MOVE IN1-IDARTNR      TO REG-UT-IDARTNR                              
009598     MOVE IN1-IDARTNR-OBJ  TO REG-UT-IDARTNR-OBJ                          
009599     MOVE IN1-KDPRODSL     TO REG-UT-KDPRODSL                             
009600     MOVE IN1-IDFKNGRP     TO REG-UT-IDFKNGRP                             
009601     MOVE IN1-BEART        TO REG-UT-BEART                                
009602     MOVE IN1-IDDISTR      TO REG-UT-IDDISTR                              
009603     PERFORM S12-ANROPA-W510MARK                                          
009604     MOVE IN1-TIFSGVV      TO REG-UT-TIFSGVV                              
009605     MOVE IN1-SUARTFSG     TO REG-UT-SUARTFSG                             
009606     MOVE IN1-SULEVANT     TO REG-UT-SULEVANT                             
009607     MOVE ZERO             TO REG-UT-KVRETUR-URSP                         
009608     MOVE ZERO             TO REG-UT-KVRETUR-GODK                         
009609     MOVE IN1-IDTABNR      TO REG-UT-IDTABNR                              
009610     MOVE ZERO             TO REG-UT-IDBYTRAP                             
009611     MOVE WC-DC-ZERO       TO REG-UT-IDDC                                 
009612                                                                          
009613     .                                                                    
009614     EJECT                                                                
009615                                                                          
009616 CD-SKAPA-REGISTER SECTION.                                               
009617     MOVE 'CD-SKAPA-REGISTER' TO WS-SEKTION                               
009618                                                                          
009619     MOVE IN2-IDARTNR      TO REG-UT-IDARTNR                              
009620     MOVE IN2-IDARTNR-OBJ  TO REG-UT-IDARTNR-OBJ                          
009621     IF IN2-IDARTNR-OBJ > ZERO                                            
009622        PERFORM S13-HMTA-PRODSL-FKNGRP                                    
009623        PERFORM S14-HMTA-BENAMNING                                        
009624     ELSE                                                                 
009625        MOVE ZERO          TO REG-UT-KDPRODSL                             
009626        MOVE SPACE            TO REG-UT-BEART                             
009627     END-IF                                                               
009628     MOVE IN2-IDDISTR      TO REG-UT-IDDISTR                              
009629     PERFORM S12-ANROPA-W510MARK                                          
009630     MOVE IN2-TIFSGVV      TO REG-UT-TIFSGVV                              
009631     MOVE IN2-IDFKNGRP     TO REG-UT-IDFKNGRP                             
009632     MOVE ZERO             TO REG-UT-SUARTFSG                             
009633     MOVE ZERO             TO REG-UT-SULEVANT                             
009634     MOVE IN2-KVRETUR-URSP TO REG-UT-KVRETUR-URSP                         
009635     MOVE IN2-KVRETUR-GODK TO REG-UT-KVRETUR-GODK                         
009636     MOVE IN2-IDTABNR      TO REG-UT-IDTABNR                              
009637     MOVE IN2-IDBYTRAP     TO REG-UT-IDBYTRAP                             
009638     MOVE IN2-IDDC         TO REG-UT-IDDC                                 
009639                                                                          
009640     .                                                                    
009641     EJECT                                                                
009642                                                                          
009643 Z-FINIT SECTION.                                                         
009644     MOVE 'Z-FINIT' TO WS-SEKTION                                         
009645     CLOSE W3715A                                                         
009646           W3715B                                                         
009647           REG-IN                                                         
009650           REG-UT                                                         
009701     SKIP2                                                                
009702     MOVE 'S' TO POSTSUM-OPKOD                                            
009710     CALL POSTSUM USING POSTSUM-PARM                                      
009800     .                                                                    
009901     EJECT                                                                
009902 S01-LAES-W3715A  SECTION.                                                
009903     MOVE 'S01-LAES-W3715A' TO WS-FIL-SEKTION                             
009904     READ W3715A INTO IN1-AREA                                            
009905     AT END                                                               
009906        MOVE HIGH-VALUE TO IN1-AREA                                       
009907        SET END-OF-W3715A TO TRUE                                         
009909     NOT AT END                                                           
009910        MOVE 'W3715A' TO POSTSUM-FDNAMN                                   
009911        MOVE 'W3715CD1' TO POSTSUM-DDNAMN2                                
009912        MOVE 'IN1'      TO POSTSUM-TRANSTYP                               
009914        CALL POSTSUM USING POSTSUM-PARM                                   
009915     END-READ                                                             
009916     .                                                                    
009917     EJECT                                                                
009918 S02-LAES-W3715B  SECTION.                                                
009919     MOVE 'S02-LAES-W3715B' TO WS-FIL-SEKTION                             
009920     READ W3715B INTO IN2-AREA                                            
009921     AT END                                                               
009922        MOVE HIGH-VALUE TO IN2-AREA                                       
009923        SET END-OF-W3715B TO TRUE                                         
009924                                                                          
009925     NOT AT END                                                           
009926        MOVE 'W3715B' TO POSTSUM-FDNAMN                                   
009927        MOVE 'W3715CD2' TO POSTSUM-DDNAMN2                                
009928        MOVE 'IN2'      TO POSTSUM-TRANSTYP                               
009930        CALL POSTSUM USING POSTSUM-PARM                                   
009931     END-READ                                                             
009932     .                                                                    
009933     EJECT                                                                
009934 S03-LAES-W01160  SECTION.                                                
009935     MOVE 'S03-LAES-W01160' TO WS-FIL-SEKTION                             
009936     READ W01160 INTO IN3-AREA                                            
009937     AT END                                                               
009938        MOVE HIGH-VALUE TO IN3-AREA                                       
009939        SET END-OF-W01160 TO TRUE                                         
009940                                                                          
009941     NOT AT END                                                           
009942        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
009943        MOVE 'W3715CD3' TO POSTSUM-DDNAMN2                                
009944        CALL POSTSUM USING POSTSUM-PARM                                   
009945     END-READ                                                             
009946     .                                                                    
009947     EJECT                                                                
009948 S04-LAES-W01174 SECTION.                                                 
009949     MOVE 'S04-LAES-W01174' TO WS-FIL-SEKTION                             
009950                                                                          
009951     READ W01174 INTO IN4-AREA                                            
009952     AT END                                                               
009953        MOVE HIGH-VALUE TO IN4-AREA                                       
009954        SET END-OF-W01174 TO TRUE                                         
009955                                                                          
009956     NOT AT END                                                           
009957        MOVE 'W01174' TO POSTSUM-FDNAMN                                   
009958        MOVE 'W3715CD4' TO POSTSUM-DDNAMN2                                
009959        CALL POSTSUM USING POSTSUM-PARM                                   
009960     END-READ                                                             
009961     .                                                                    
009962     EJECT                                                                
009963 S05-LAS-REG-IN SECTION.                                                  
009964     MOVE 'S05-LAS-REG-IN' TO WS-FIL-SEKTION                              
009965     SKIP2                                                                
009966     READ REG-IN INTO REG-IN-AREA                                         
009967     AT END                                                               
009968        MOVE HIGH-VALUE TO REG-IN-AREA                                    
009969        SET END-OF-REG-IN TO TRUE                                         
009970     END-READ                                                             
009971     IF END-OF-REG-IN                                                     
009972       MOVE 'REG-IN'            TO POSTSUM-FDNAMN                         
009973       MOVE 'W3715CD5'          TO POSTSUM-DDNAMN2                        
009974       MOVE 'REGIN'             TO POSTSUM-TRANSTYP                       
009975       CALL POSTSUM   USING POSTSUM-PARM                                  
009976     END-IF                                                               
009977     .                                                                    
009980     EJECT                                                                
010210 S11-SKRIV-REG-UT SECTION.                                                
010211     MOVE 'S11-SKRIV-REG-UT' TO WS-FIL-SEKTION                            
010220     SKIP2                                                                
010230     WRITE REG-UT-POST FROM REG-UT-AREA                                   
010240     MOVE 'REG-UT'               TO POSTSUM-FDNAMN                        
010241     MOVE 'REGUT'                TO POSTSUM-TRANSTYP                      
010250     MOVE 'W3715CD6'             TO POSTSUM-DDNAMN2                       
010270     CALL POSTSUM      USING POSTSUM-PARM                                 
010280     .                                                                    
010290     EJECT                                                                
010291 S12-ANROPA-W510MARK SECTION.                                             
010292     MOVE 'S12-ANROPA-W510MARK' TO WS-SUB-SEKTION                         
010293                                                                          
010294     MOVE REG-UT-IDDISTR  TO MARK-IDDISTR                                 
010295     MOVE +0           TO MARK-KDCALL                                     
010296     CALL W510MARK USING   MARK-W510MARK                                  
010297     IF MARK-KDSVAR = '0'                                                 
010298        MOVE MARK-KDMARK-BUDG TO REG-UT-KDMARK-BUDG                       
010299        MOVE MARK-BEMARKN TO REG-UT-BEMARKN                               
010300     ELSE                                                                 
010301        MOVE SPACE            TO REG-UT-BEMARKN                           
010302        MOVE ZERO             TO REG-UT-KDMARK-BUDG                       
010303     END-IF                                                               
010304                                                                          
010305     .                                                                    
010306     EJECT                                                                
010307                                                                          
010308 S13-HMTA-PRODSL-FKNGRP SECTION.                                          
010309     MOVE 'S13-HMTA-PRODSL-FKNGRP' TO WS-SUB-SEKTION                      
010310                                                                          
010311     PERFORM UNTIL END-OF-W01160                                          
010312             OR IN2-IDARTNR-OBJ = IN3-CLAG-IDARTNR                        
010313             OR IN2-IDARTNR-OBJ < IN3-CLAG-IDARTNR                        
010314                PERFORM S03-LAES-W01160                                   
010315     END-PERFORM                                                          
010316     IF IN2-IDARTNR-OBJ = IN3-CLAG-IDARTNR                                
010317         MOVE IN3-CLAG-KDPRODSL     TO REG-UT-KDPRODSL                    
010318     ELSE                                                                 
010319        MOVE ZERO              TO REG-UT-KDPRODSL                         
010320     END-IF                                                               
010321                                                                          
010322     .                                                                    
010323     EJECT                                                                
010324 S14-HMTA-BENAMNING SECTION.                                              
010325     MOVE 'S14-HMTA-BENAMNING' TO WS-SUB-SEKTION                          
010326                                                                          
010327     PERFORM UNTIL END-OF-W01174                                          
010328             OR IN2-IDARTNR-OBJ = IN4-IDARTNR                             
010329             OR IN2-IDARTNR-OBJ < IN4-IDARTNR                             
010330                PERFORM S04-LAES-W01174                                   
010331     END-PERFORM                                                          
010332     IF IN2-IDARTNR-OBJ = IN4-IDARTNR                                     
010333        MOVE IN4-BEART(8)  TO REG-UT-BEART                                
010334     ELSE                                                                 
010335        MOVE SPACE             TO REG-UT-BEART                            
010336     END-IF                                                               
010337     .                                                                    
010338     EJECT                                                                
010339                                                                          
010340 S99-ABEND SECTION.                                                       
010400                                                                          
010501     SKIP2                                                                
010502     MOVE 'S' TO POSTSUM-OPKOD                                            
010510     CALL POSTSUM USING POSTSUM-PARM                                      
010600     CALL ABEND USING RKOD-ABEND                                          
010700     .                                                                    
