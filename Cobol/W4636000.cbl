000010                                                                          
000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4636000.                                                
000400 AUTHOR.         BO SVENSSON.                                             
000500 DATE-WRITTEN.   00/11/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000810*                                                                         
000900*    FUNKTION:                                                            
001000*        TAR IN DIRECT BUSUNESS FIL OCH NUMRERAR POSTER                   
001200*        FRÅN 1 OCH UPPÅT FÖR ATT I SENARE STEG VARA SÄKER                
001300*        PÅ ORDNINGEN.                                                    
001400*                                                                         
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002701     SKIP2                                                                
002702*          --- FIL FRÅN DIRECT BUSINESS                                   
002703     SELECT W46360                     ASSIGN TO W46360D1.                
002704     SKIP2                                                                
002705*          --- NUMMRERAD, DIRECT BUSINESSFIL                              
002710     SELECT W46361                     ASSIGN TO W46360D2.                
002940     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003301     SKIP3                                                                
003302 FD  W46360                                                               
003303     RECORDING       F                                                    
003304     BLOCK CONTAINS  0.                                                   
003305                                                                          
003306 01  IN-POST         PIC X(160).                                          
003307                                                                          
003341     SKIP3                                                                
003342 FD  W46361                                                               
003343     RECORDING       F                                                    
003344     BLOCK CONTAINS  0.                                                   
003345                                                                          
003350 01  UT-POST         PIC X(190).                                          
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W4636000'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000     SKIP2                                                                
004010 77  W-IX                        PIC 9(3)    VALUE ZERO.                  
004020     SKIP2                                                                
004100 01  FELTEXT.                                                             
004101     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004102     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004103                                                                          
004104 77  W46360-EOF-SW               PIC X       VALUE 'N'.                   
004110     88  END-OF-W46360                       VALUE 'J'.                   
004200     EJECT                                                                
004231                                                                          
004232 77  W-NUMMER                    PIC 9(6)    VALUE ZERO.                  
004233 77  W-SPAR-SERIALNO             PIC 9(7)    VALUE ZERO.                  
004234 77  WS-DAGENS-DATUM             PIC 9(6)    VALUE ZERO.                  
004235 77  WS-TIDPUNKT                 PIC 9(8)    VALUE ZERO.                  
004240     EJECT                                                                
004900 01  DYNAMISKA-SUBPROGRAM.                                                
005000*                                                                         
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005300     SKIP2                                                                
005400*    --- PARAMETRAR TILL ABEND                                            
005500                                                                          
005600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006300     EJECT                                                                
006307*    --- PARAMETRAR TILL POSTSUM                                          
006308*                                                                         
006310*01  -COPY W0005   -PRE  POSTSUM-                                         
006320     EJECT                                                                
006330 01  FILLER                      PIC X(16)  VALUE 'W463RSUM-AREA'.        
006340*   -COPY W463RSUM                                                        
006501     EJECT                                                                
006502 01  IN-AREA-START               PIC X(24)   VALUE                        
006503                                 'IN-AREA-START  '.                       
006504     SKIP2                                                                
006505 01  W-IN-POST.                                                           
006506   03 FILLER                     PIC X(19).                               
006507   03 W-IN-TYP                   PIC X(1).                                
006508   03 FILLER                     PIC X(140).                              
006563     EJECT                                                                
006564 01  UT-AREA-START               PIC X(24)   VALUE                        
006565                                 'UT-AREA-START  '.                       
006566     SKIP2                                                                
006567 01  W-UT-POST.                                                           
006568   03 W-UTP-SERIALNO             PIC 9(7).                                
006569   03 W-UTP-DATUM                PIC 9(6).                                
006570   03 W-UTP-TID                  PIC 9(8).                                
006571   03 W-UTP-FELKOD               PIC 9(3).                                
006572   03 W-UTP-NUMMER               PIC 9(6).                                
006573   03 W-UTP-INPOST.                                                       
006574     05 W-UT-ID                  PIC X(4).                                
006575     05 W-UT-FILEDEF             PIC X(2).                                
006576     05 W-UT-SENDER              PIC X(3).                                
006577     05 W-UT-RECIEVER            PIC X(3).                                
006578     05 W-UT-SERIALNO            PIC X(7).                                
006579     05 W-UT-TYP                 PIC X.                                   
006580     05 W-UT-RESTEN.                                                      
006581       07 W-UT-ANT OCCURS 24     PIC 9(5).                                
006582       07 FILLER                 PIC X(20).                               
006590                                                                          
006640     EJECT                                                                
006700 PROCEDURE DIVISION.                                                      
006800 MAIN SECTION.                                                            
007000     SKIP2                                                                
007100                                                                          
007200     PERFORM A-INIT                                                       
007310     PERFORM S01-LAES-W46360                                              
007311     IF NOT END-OF-W46360                                                 
007312       MOVE W-IN-POST TO W-UTP-INPOST                                     
007313       IF W-UT-SERIALNO NOT NUMERIC                                       
007314       OR W-UT-SERIALNO NOT > ZERO                                        
007315         MOVE ZERO TO W-SPAR-SERIALNO                                     
007316         MOVE 101  TO W-UTP-FELKOD                                        
007317       ELSE                                                               
007318         MOVE W-UT-SERIALNO TO W-SPAR-SERIALNO                            
007320         IF W-UT-TYP NOT = 'A'                                            
007330           MOVE 102 TO W-UTP-FELKOD                                       
007340         ELSE                                                             
007341           IF  W-UT-ID NOT = 'VCON'                                       
007342           AND W-UT-ID NOT = 'VMER'                                       
007342           AND W-UT-ID NOT = 'VSER'                                       
007343           AND W-UT-ID NOT = 'VTYR'                                       
007345             MOVE 103 TO W-UTP-FELKOD                                     
007346           ELSE                                                           
007347             IF W-UT-FILEDEF NOT = '01'                                   
007348               MOVE 104 TO W-UTP-FELKOD                                   
007349             ELSE                                                         
007350               IF W-UT-SENDER NOT = 'IMS'                                 
007351                 MOVE 105 TO W-UTP-FELKOD                                 
007352               ELSE                                                       
007353                 IF W-UT-RECIEVER NOT = 'VMD'                             
007354                   MOVE 106 TO W-UTP-FELKOD                               
007360                 END-IF                                                   
007361               END-IF                                                     
007362             END-IF                                                       
007363           END-IF                                                         
007364         END-IF                                                           
007365       END-IF                                                             
007370                                                                          
007380       PERFORM S01-LAES-W46360                                            
007400       PERFORM UNTIL END-OF-W46360                                        
007471                                                                          
007472         ADD 1       TO W-NUMMER                                          
007473                                                                          
007474         IF W-NUMMER > 999998                                             
007475             MOVE 'FÖR MÅNGA POSTER' TO FELTEXT-STR                       
007476             DISPLAY FELTEXT                                              
007477             PERFORM S99-ABEND                                            
007478         END-IF                                                           
007479                                                                          
007480         MOVE W-SPAR-SERIALNO TO W-UTP-SERIALNO                           
007481         MOVE WS-DAGENS-DATUM TO W-UTP-DATUM                              
007482         MOVE WS-TIDPUNKT     TO W-UTP-TID                                
007483         MOVE W-NUMMER        TO W-UTP-NUMMER                             
007800                                                                          
007900         PERFORM S10-SKRIV-W46361                                         
008000                                                                          
008100         MOVE W-IN-POST TO W-UTP-INPOST                                   
008101         MOVE ZERO      TO W-UTP-FELKOD                                   
008111                                                                          
008120         IF W-UT-SERIALNO NOT NUMERIC                                     
008130         OR W-UT-SERIALNO NOT > ZERO                                      
008131         OR (W-SPAR-SERIALNO > 0                                          
008132         AND W-UT-SERIALNO NOT = W-SPAR-SERIALNO)                         
008140           MOVE 101 TO W-UTP-FELKOD                                       
008150         ELSE                                                             
008191           IF  W-UT-ID NOT = 'VCON'                                       
008192           AND W-UT-ID NOT = 'VMER'                                       
008192           AND W-UT-ID NOT = 'VSER'                                       
008193           AND W-UT-ID NOT = 'VTYR'                                       
008194             MOVE 103 TO W-UTP-FELKOD                                     
008195           ELSE                                                           
008196             IF W-UT-FILEDEF NOT = '01'                                   
008197               MOVE 104 TO W-UTP-FELKOD                                   
008198             ELSE                                                         
008200               IF W-UT-RECIEVER NOT = 'VMD'                               
008201                 MOVE 106 TO W-UTP-FELKOD                                 
008204               END-IF                                                     
008206             END-IF                                                       
008207           END-IF                                                         
008208         END-IF                                                           
008209         PERFORM S01-LAES-W46360                                          
008210       END-PERFORM                                                        
008220                                                                          
008250       IF W-UT-TYP NOT = 'Z'                                              
008260         MOVE 102 TO W-UTP-FELKOD                                         
008270       ELSE                                                               
008271         MOVE 1 TO W-IX                                                   
008272         PERFORM UNTIL W-IX > POSTTYP-MAX-IX                              
008273            SET POSTTYP-IX TO W-IX                                        
008274            IF  W-UT-ANT(W-IX) NOT NUMERIC                                
008275            OR  POSTTYP-RAKNARE(POSTTYP-IX) NOT =                         
008276                W-UT-ANT(W-IX)                                            
008277              MOVE 102 TO W-UTP-FELKOD                                    
008278            END-IF                                                        
008279            ADD 1 TO W-IX                                                 
008305         END-PERFORM                                                      
008306       END-IF                                                             
008400                                                                          
008410       ADD 1       TO W-NUMMER                                            
008420                                                                          
008430       IF W-NUMMER > 999998                                               
008440           MOVE 'FÖR MÅNGA POSTER' TO FELTEXT-STR                         
008450           DISPLAY FELTEXT                                                
008460           PERFORM S99-ABEND                                              
008470       END-IF                                                             
008480                                                                          
008481       MOVE W-SPAR-SERIALNO TO W-UTP-SERIALNO                             
008482       MOVE WS-DAGENS-DATUM TO W-UTP-DATUM                                
008483       MOVE WS-TIDPUNKT     TO W-UTP-TID                                  
008490       MOVE W-NUMMER        TO W-UTP-NUMMER                               
008492                                                                          
008493       PERFORM S10-SKRIV-W46361                                           
008495     END-IF                                                               
008496                                                                          
008500     PERFORM Z-FINIT                                                      
008600                                                                          
008700     MOVE ZERO TO RETURN-CODE                                             
008800     GOBACK                                                               
008900     .                                                                    
009000     EJECT                                                                
009100 A-INIT SECTION.                                                          
009201                                                                          
009210     OPEN INPUT  W46360                                                   
009301                                                                          
009310     OPEN OUTPUT W46361                                                   
009320                                                                          
009400     SKIP2                                                                
009610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009620     MOVE SPACE TO W-UT-POST                                              
009630     MOVE ZERO  TO W-SPAR-SERIALNO                                        
009640     MOVE ZERO  TO W-NUMMER                                               
009650     MOVE ZERO  TO W-UTP-FELKOD                                           
009660     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
009670     ACCEPT WS-TIDPUNKT                   FROM TIME                       
009700     .                                                                    
009701     EJECT                                                                
010204 Z-FINIT SECTION.                                                         
010205                                                                          
010212     CLOSE W46360                                                         
010213           W46361                                                         
010215     SKIP2                                                                
010216     MOVE 'S' TO POSTSUM-OPKOD                                            
010220     CALL POSTSUM USING POSTSUM-PARM                                      
010300     .                                                                    
010301     EJECT                                                                
010302 S01-LAES-W46360  SECTION.                                                
010303     READ W46360 INTO W-IN-POST                                           
010304     AT END                                                               
010305        MOVE HIGH-VALUE   TO W-IN-POST                                    
010306        SET END-OF-W46360 TO TRUE                                         
010307                                                                          
010308     NOT AT END                                                           
010309                                                                          
010310        SEARCH ALL POSTTYP-INGANG                                         
010311           AT END                                                         
010312              CONTINUE                                                    
010318           WHEN POSTTYP-SOK(POSTTYP-IX) = W-IN-TYP                        
010319              ADD 1 TO POSTTYP-RAKNARE(POSTTYP-IX)                        
010321        END-SEARCH                                                        
010322                                                                          
010323        MOVE 'W46360'   TO POSTSUM-FDNAMN                                 
010324        MOVE 'W46360D1' TO POSTSUM-DDNAMN2                                
010325        MOVE SPACE      TO POSTSUM-TRANSTYP                               
010326        CALL POSTSUM USING POSTSUM-PARM                                   
010327     END-READ                                                             
010330     .                                                                    
010401     EJECT                                                                
010402 S10-SKRIV-W46361 SECTION.                                                
010403                                                                          
010407     WRITE UT-POST FROM W-UT-POST                                         
010408                                                                          
010409     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
010410     MOVE 'W46361'   TO POSTSUM-FDNAMN                                    
010411     MOVE 'W46360D2' TO POSTSUM-DDNAMN2                                   
010412     CALL POSTSUM USING POSTSUM-PARM                                      
010420     .                                                                    
010741     EJECT                                                                
010750 S99-ABEND SECTION.                                                       
010800                                                                          
010901     SKIP2                                                                
010902     MOVE 'S' TO POSTSUM-OPKOD                                            
010910     CALL POSTSUM USING POSTSUM-PARM                                      
011000     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
011100     .                                                                    
