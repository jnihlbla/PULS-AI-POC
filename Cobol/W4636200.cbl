001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4636200.                                                
001200 AUTHOR.         BO SVENSSON.                                             
001300 DATE-WRITTEN.   00/11/24.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001610*        DIRECT BUSINESS.                                                 
001700*        KONTROLLERAR BILLINGTRANSAR MOT DEALER KUNDREG.                  
001800*        KOMPLETTERAR BILLINGTRANSAR MED PRODUKTSLAG,                     
001900*        FUNKTIONSGRUPP OCH LEVERANTÖRSNUMMER.                            
002100*                                                                         
002202*        PROGRAMMET LÄSER      WDB7                                       
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- FIL FRÅN DIREKTLEVERANTÖR                                  
003503     SELECT W46366                     ASSIGN TO W46362D1.                
003504     SKIP2                                                                
003505*          --- TRANSAR KLARA FÖR SPLITT TILL SÄLJBOLAG                    
003506     SELECT W46367                     ASSIGN TO W46362D2.                
003507     SKIP2                                                                
003508*          --- FELPOSTER                                                  
003510     SELECT W46365                     ASSIGN TO W46362D3.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W46366                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105                                                                          
004106 01  IN-POST           PIC X(196).                                        
004107     SKIP3                                                                
004108 FD  W46367                                                               
004109     RECORDING       F                                                    
004110     BLOCK CONTAINS  0.                                                   
004111                                                                          
004112 01  UTOK-POST         PIC X(220).                                        
004113     SKIP3                                                                
004114 FD  W46365                                                               
004115     RECORDING       F                                                    
004116     BLOCK CONTAINS  0.                                                   
004117                                                                          
004120 01  UTFP-POST         PIC X(190).                                        
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4636200'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004901                                                                          
004902 77  W-SPAR-DEALER               PIC X(7)    VALUE SPACE.                 
004903 77  W-SPAR-LAND                 PIC X(3)    VALUE SPACE.                 
004904                                                                          
004914 77  W46366-EOF-SW               PIC X       VALUE 'N'.                   
004915     88  END-OF-W46366                       VALUE 'J'.                   
004920                                                                          
004930 77  W-DEALER-SAKNAS-SW          PIC X       VALUE 'N'.                   
004940     88  W-DEALER-SAKNAS                     VALUE 'J'.                   
004950                                                                          
005000     EJECT                                                                
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005520                                                                          
005600     SKIP2                                                                
005601 01  W-PTYP-SOK.                                                          
005602     03  W-PTYP-PROG             PIC X(4).                                
005603     03  W-PTYP-TYP              PIC X.                                   
005604                                                                          
005605     SKIP2                                                                
005606 01  W-SUPL-SOK.                                                          
005607     03  W-SUPL-PROG             PIC X(4).                                
005608     03  W-SUPL-LAND             PIC X(3).                                
005609     03  W-SUPL-SUP              PIC X(2).                                
005610     SKIP2                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007320     EJECT                                                                
007330*    --- PARAMETRAR TILL POSTSUM                                          
007340*                                                                         
007350*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007502*    --- TABELL FÖR FUNKTIONSGRUPP                                        
007503*                                                                         
007504*01  -COPY W463FGRP                                                       
007505     EJECT                                                                
007506*    --- TABELL FÖR SUPPLIERKOD                                           
007507*                                                                         
007508*01  -COPY W463SUPL                                                       
007509     EJECT                                                                
007510*    --- TABELL FÖR LANDKOD                                               
007511*                                                                         
007512*01  -COPY W463LAND                                                       
007513     EJECT                                                                
007514 01  IN-AREA-START               PIC X(24)   VALUE                        
007515                                 'IN-AREA-START  '.                       
007516     SKIP2                                                                
007517                                                                          
007518 01  IN-AREA.                                                             
007519     03 IN-S-DEL                 PIC X(30).                               
007520     03 IN-URSP-POST             PIC X(160).                              
007521*    03 -COPY WINVBBB0 -RED IN-URSP-POST                                  
007533     03 IN-PRODTYP               PIC X(1).                                
007534     03 IN-BASEDISC              PIC X(5).                                
007535     EJECT                                                                
007536 01  UTOK-AREA-START             PIC X(24)   VALUE                        
007537                                 'UTOK-AREA-START  '.                     
007538     SKIP2                                                                
007539                                                                          
007540 01  UTOK-AREA.                                                           
007541     03 UTOK-S-DEL               PIC X(30).                               
007542     03 UTOK-URSP-POST           PIC X(160).                              
007543     03 UTOK-PRODTYP             PIC X(1).                                
007544     03 UTOK-BONUSBASE           PIC 9(11).                               
007545     03 UTOK-KDPRODSL            PIC 9(2).                                
007546     03 UTOK-IDFKNGRP            PIC 9(4).                                
007547     03 UTOK-IDPARTNR            PIC 9(7).                                
007548     03 UTOK-BASEDISC            PIC X(5).                                
007551     EJECT                                                                
007552 01  UTFP-AREA-START             PIC X(24)   VALUE                        
007553                                 'UTFP-AREA-START  '.                     
007554     SKIP2                                                                
007555                                                                          
007560 01  UTFP-AREA.                                                           
007570     03 UTFP-S-DEL.                                                       
007571       05  UTFP-1-21             PIC X(21).                               
007572       05  UTFP-FELKOD           PIC 9(3).                                
007573       05  UTFP-25-30            PIC X(6).                                
007580     03 UTFP-URSP-POST           PIC X(160).                              
007600     EJECT                                                                
007700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800*                                                                         
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  NYCKLAR-TILL-DLI.                                                    
008301     03  W-WDB7A1KY-X.                                                    
008302         05  W-IDDEALER-B7A      PIC X(6)    VALUE SPACE.                 
008303         05  W-IDLANDX2-B7A      PIC X(2)    VALUE SPACE.                 
008315                                                                          
008400     SKIP2                                                                
008500*    --- STATUS-KOD FRÅN IMS                                              
008600 01  STATUS-WS                   PIC XX.                                  
008700     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009000     SKIP2                                                                
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010200 01  FILLER                      PIC X(16)   VALUE 'WDB701-AREA'.         
010210 01  DLI-IO-AREA-B701.                                                    
010211*    03  -COPY WDB701                                                     
010540     EJECT                                                                
010600 LINKAGE SECTION.                                                         
010700                                                                          
010801     EJECT                                                                
010805*01  -COPY W0008  -PRE WDB7AQ-                                            
010806     05  FILLER                  PIC X.                                   
011002     EJECT                                                                
011003 PROCEDURE DIVISION  USING WDB7AQ-PCB.                                    
011005 MAIN SECTION.                                                            
011006     ENTRY 'DLITCBL' USING WDB7AQ-PCB.                                    
011300                                                                          
011400     PERFORM A-INIT                                                       
011500                                                                          
011610     PERFORM S01-LAES-W46366                                              
011611                                                                          
011620     IF NOT END-OF-W46366                                                 
011700       PERFORM UNTIL END-OF-W46366                                        
011710         MOVE B0-CUST-ORDERING TO W-IDDEALER-B7A                          
011711                                  W-SPAR-DEALER                           
011712         MOVE B0-BILL-LOC      TO W-SPAR-LAND                             
011713         SEARCH ALL LAND-X2-ING                                           
011714            AT END                                                        
011715               MOVE SPACE   TO W-IDLANDX2-B7A                             
011716            WHEN LAND-SOK(LAND-IX) = B0-BILL-LOC                          
011717               MOVE LAND-IDLANDX2(LAND-IX)                                
011718                            TO W-IDLANDX2-B7A                             
011719         END-SEARCH                                                       
011720*------ FIX FÖR TEST                                                      
011721*        MOVE 'EI550'          TO W-IDDEALER-B7A                          
011721*PL      MOVE '001'          TO W-IDDEALER-B7A                            
011721*BE      MOVE '000004'       TO W-IDDEALER-B7A                            
011722*        MOVE 'P0'             TO W-IDLANDX2-B7A                          
011723                                                                          
011724         IF B0-RECORDTYPE NOT = 'P'                                       
011725          PERFORM IMS-GU-WDB7ASEQ                                         
011726          IF SEGMENT-SAKNAS                                               
011727            MOVE JA          TO W-DEALER-SAKNAS-SW                        
011728          ELSE                                                            
011729            MOVE NEJ         TO W-DEALER-SAKNAS-SW                        
011730          END-IF                                                          
011731         ELSE                                                             
011732           MOVE NEJ          TO W-DEALER-SAKNAS-SW                        
011733         END-IF                                                           
011740                                                                          
011762         PERFORM UNTIL END-OF-W46366                                      
011763          OR B0-CUST-ORDERING NOT = W-SPAR-DEALER                         
011764          OR B0-BILL-LOC      NOT = W-SPAR-LAND                           
011765                                                                          
011766           IF W-DEALER-SAKNAS                                             
011768             MOVE IN-S-DEL       TO UTFP-S-DEL                            
011769             MOVE IN-URSP-POST   TO UTFP-URSP-POST                        
011770             MOVE 903            TO UTFP-FELKOD                           
011771             PERFORM S12-SKRIV-W46365                                     
011772           ELSE                                                           
011803                                                                          
011804             MOVE SPACE          TO UTOK-AREA                             
011805             MOVE IN-S-DEL       TO UTOK-S-DEL                            
011806             MOVE IN-URSP-POST   TO UTOK-URSP-POST                        
011807             MOVE IN-PRODTYP     TO UTOK-PRODTYP                          
011808             MOVE ZERO           TO UTOK-BONUSBASE                        
011809             MOVE ZERO           TO UTOK-KDPRODSL                         
011810             MOVE ZERO           TO UTOK-IDFKNGRP                         
011811             MOVE ZERO           TO UTOK-IDPARTNR                         
011812             MOVE IN-BASEDISC    TO UTOK-BASEDISC                         
011813                                                                          
011814             IF B0-LINETYPE = 2                                           
011815               MOVE 12           TO UTOK-KDPRODSL                         
011816               IF IN-PRODTYP = 'F'                                        
011817               OR IN-PRODTYP = 'G'                                        
011818               OR IN-PRODTYP = 'H'                                        
011819                 MOVE B0-PRODQ-2 TO UTOK-BONUSBASE                        
011820               ELSE                                                       
011821                 IF IN-PRODTYP = '1'                                      
011822                 OR IN-PRODTYP = '2'                                      
011823                 OR IN-PRODTYP = 'E'                                      
011824                   MOVE B0-LINE-VALUE                                     
011825                                 TO UTOK-BONUSBASE                        
011826                 END-IF                                                   
011827               END-IF                                                     
011828               MOVE B0-PROGRAM   TO W-PTYP-PROG                           
011829               MOVE IN-PRODTYP   TO W-PTYP-TYP                            
011830               SEARCH ALL PTYP-FGRP-ING                                   
011831                  AT END                                                  
011832                     MOVE ZERO   TO UTOK-IDFKNGRP                         
011833                  WHEN PTYP-SOK(PTYP-IX) = W-PTYP-SOK                     
011834                     MOVE PTYP-FGRP(PTYP-IX)                              
011835                                 TO UTOK-IDFKNGRP                         
011836               END-SEARCH                                                 
011838                                                                          
011839               MOVE B0-PROGRAM    TO W-SUPL-PROG                          
011840               MOVE B0-BILL-LOC   TO W-SUPL-LAND                          
011841               MOVE B0-SENDLOC(1:2) TO W-SUPL-SUP                         
011842               SEARCH ALL SUPL-LEVNR-ING                                  
011843                  AT END                                                  
011844                     MOVE ZERO    TO UTOK-IDPARTNR                        
011845                  WHEN SUPL-SOK(SUPL-IX) = W-SUPL-SOK                     
011846                     MOVE SUPL-LEVNR(SUPL-IX)                             
011847                                  TO UTOK-IDPARTNR                        
011848               END-SEARCH                                                 
011849             END-IF                                                       
011850             PERFORM S11-SKRIV-W46367                                     
011860           END-IF                                                         
011900           PERFORM S01-LAES-W46366                                        
012500         END-PERFORM                                                      
012600       END-PERFORM                                                        
012620     END-IF                                                               
012700                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013501                                                                          
013510     OPEN INPUT  W46366                                                   
013601                                                                          
013602     OPEN OUTPUT W46367                                                   
013610                 W46365                                                   
013700                                                                          
013910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014100     .                                                                    
014290     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014401     CLOSE W46366                                                         
014402           W46367                                                         
014410           W46365                                                         
014501     SKIP2                                                                
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014701     EJECT                                                                
014702 S01-LAES-W46366  SECTION.                                                
014703     READ W46366 INTO IN-AREA                                             
014704     AT END                                                               
014705        MOVE HIGH-VALUE TO IN-AREA                                        
014706        SET END-OF-W46366 TO TRUE                                         
014707                                                                          
014708     NOT AT END                                                           
014709        MOVE 'W46366' TO POSTSUM-FDNAMN                                   
014710        MOVE 'W46362D1' TO POSTSUM-DDNAMN2                                
014713        MOVE SPACE TO POSTSUM-TRANSTYP                                    
014714        CALL POSTSUM USING POSTSUM-PARM                                   
014715     END-READ                                                             
014720     .                                                                    
014801     EJECT                                                                
014802 S11-SKRIV-W46367 SECTION.                                                
014803                                                                          
014804     WRITE UTOK-POST FROM UTOK-AREA                                       
014805                                                                          
014806     MOVE SPACE TO POSTSUM-TRANSTYP                                       
014807     MOVE 'W46367' TO POSTSUM-FDNAMN                                      
014808     MOVE 'W46362D2' TO POSTSUM-DDNAMN2                                   
014809     CALL POSTSUM USING POSTSUM-PARM                                      
014810     .                                                                    
014811     EJECT                                                                
014812 S12-SKRIV-W46365 SECTION.                                                
014813                                                                          
014817     WRITE UTFP-POST FROM UTFP-AREA                                       
014818                                                                          
014819     MOVE SPACE TO POSTSUM-TRANSTYP                                       
014820     MOVE 'W46365' TO POSTSUM-FDNAMN                                      
014821     MOVE 'W46362D3' TO POSTSUM-DDNAMN2                                   
014822     CALL POSTSUM USING POSTSUM-PARM                                      
014830     .                                                                    
015600     EJECT                                                                
015700* --- IMS SEKTIONER ---                                                   
015800     SKIP3                                                                
015810 IMS-GU-WDB7ASEQ SECTION.                                                 
015820                                                                          
015830     STRING 'WDB701  (WDB7ASEQ =' W-WDB7A1KY-X ')'                        
015840          DELIMITED BY SIZE INTO SSA1                                     
015850     MOVE '  GE' TO GODK-STATUSKODER                                      
015860     CALL CBLTDLI USING GU WDB7AQ-PCB DLI-IO-AREA-B701 SSA1               
015870     MOVE WDB7AQ-STATUS-CODE TO STATUS-WS                                 
015880     PERFORM IMS-STATUSKONTROLL                                           
015890     .                                                                    
016080     EJECT                                                                
016100 IMS-STATUSKONTROLL SECTION.                                              
016200                                                                          
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GODK-STATUS                                                   
016500       AT END                                                             
016600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016700           DELIMITED BY SIZE INTO FELTEXT                                 
016800         DISPLAY FELTEXT                                                  
016900         CALL FELLOG                                                      
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    
