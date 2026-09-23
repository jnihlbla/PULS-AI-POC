000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1142600.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   JANUARI 2005                                             
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER FIL FRÅN FLIT OCH SKAPAR REGISTER.                         
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
002502*          --- ARTINFO FRÅN FLIT                                          
002503     SELECT INFIL                      ASSIGN TO W11426D1.                
002504     SKIP2                                                                
002508*          --- REGISTER                                                   
002510     SELECT W11426                     ASSIGN TO W11426D2.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900                                                                          
003000 FILE SECTION.                                                            
003101     SKIP2                                                                
003102 FD  INFIL                                                                
003103     RECORDING       F                                                    
003104     BLOCK CONTAINS  0.                                                   
003105     SKIP3                                                                
003107 01  IN-POST         PIC X(100).                                          
003110     SKIP3                                                                
003120 FD  W11426                                                               
003121     RECORDING       F                                                    
003122     BLOCK CONTAINS  0.                                                   
003130*01  POST -COPY W11426  -PRE  UT-  -L.                                    
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W1142600'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
003810 77  IX-MAX                      PIC S9(3)   VALUE +15.                   
003820 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
003900 77  WS-IDARTNR-SPAR             PIC 9(9)    VALUE ZERO.                  
003907                                                                          
003908 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
003909     88  END-OF-INFIL                        VALUE 'J'.                   
003910                                                                          
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005101     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005200                                                                          
005300*    --- PARAMETRAR TILL ABEND                                            
005500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005800                                                                          
005900 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006210     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007101     EJECT                                                                
007202 01  IN-AREA-START                PIC X(24)   VALUE                       
007203                                 'IN-AREA-START  '.                       
007207 01  IN-AREA.                                                             
007208     03  IN-REC-TYPE              PIC X(3).                               
007209     03  IN-IDARTNR               PIC 9(8).                               
007210     03  IN-RESTEN                PIC X(31).                              
007222                                                                          
007224*01  AREA -COPY W11424 -PRE IN73- -RED IN-AREA                            
007234     EJECT                                                                
007264 01  UT-AREA-START               PIC X(24)   VALUE                        
007265                                 'UT-AREA-START  '.                       
007270*01  AREA -COPY W11426 -PRE UT-                                           
007300     EJECT                                                                
007400 PROCEDURE DIVISION.                                                      
007500 MAIN SECTION.                                                            
007800                                                                          
007900     PERFORM A-INIT                                                       
008000                                                                          
008001     PERFORM S01-LAES-INFIL                                               
008060                                                                          
008100     PERFORM UNTIL END-OF-INFIL                                           
008101        EVALUATE TRUE                                                     
008102           WHEN IN-REC-TYPE = 'F71'                                       
008103              IF IN-IDARTNR NOT = WS-IDARTNR-SPAR                         
008105                 IF SKRIV-SW = JA                                         
008107                    MOVE WS-IDARTNR-SPAR TO UT-IDARTNR                    
008108                    PERFORM S11-SKRIV-W11426                              
008109                    MOVE NEJ TO SKRIV-SW                                  
008110                    PERFORM S10-NOLLSTALL                                 
008111                    MOVE +1 TO IX                                         
008113                 END-IF                                                   
008114                 MOVE IN-IDARTNR TO WS-IDARTNR-SPAR                       
008115              END-IF                                                      
008144           WHEN IN-REC-TYPE = 'F73'                                       
008145              PERFORM B-SPARA-PTYP73                                      
009000        END-EVALUATE                                                      
009010        PERFORM S01-LAES-INFIL                                            
009020     END-PERFORM                                                          
009030                                                                          
009040     IF SKRIV-SW = JA                                                     
009050        MOVE WS-IDARTNR-SPAR TO UT-IDARTNR                                
009060        PERFORM S11-SKRIV-W11426                                          
009080     END-IF                                                               
009100                                                                          
009200     PERFORM Z-FINIT                                                      
009400     MOVE ZERO TO RETURN-CODE                                             
009500     GOBACK                                                               
009600     .                                                                    
009700     EJECT                                                                
009800 A-INIT SECTION.                                                          
009901                                                                          
009902     OPEN INPUT  INFIL                                                    
010010     OPEN OUTPUT W11426                                                   
010700                                                                          
010711     PERFORM S10-NOLLSTALL                                                
010712     MOVE ZERO TO WS-IDARTNR-SPAR                                         
010713     MOVE +1 TO IX                                                        
010714     MOVE NEJ TO SKRIV-SW                                                 
010720     .                                                                    
010800     EJECT                                                                
010870 B-SPARA-PTYP73 SECTION.                                                  
010880                                                                          
010882     IF IN73-IDSTEKN      = SPACE                                         
010883     AND IN73-UPG-WEEK    = SPACE                                         
010884     AND IN73-FLUPG       = SPACE                                         
010885     AND IN73-TPD-WEEK    = SPACE                                         
010886     AND IN73-KDTPD       = SPACE                                         
010887        CONTINUE                                                          
010888     ELSE                                                                 
010892        IF IN73-UPG-WEEK = SPACE                                          
010893           MOVE ZERO TO IN73-UPG-WEEK                                     
010894        END-IF                                                            
010895        IF IN73-TPD-WEEK = SPACE                                          
010896           MOVE ZERO TO IN73-TPD-WEEK                                     
010897        END-IF                                                            
010899        IF  IN73-UPG-WEEK NUMERIC                                         
010900        AND IN73-TPD-WEEK NUMERIC                                         
010901           MOVE IN73-IDLEVNR      TO UT-IDLEVNR(IX)                       
010902           MOVE IN73-IDSTEKN      TO UT-IDSTEKN(IX)                       
010903           MOVE IN73-UPG-WEEK     TO UT-DAAAVV-UPG(IX)                    
010904           MOVE IN73-FLUPG        TO UT-FLUPG(IX)                         
010905           MOVE IN73-TPD-WEEK     TO UT-DAAAVV-TPD(IX)                    
010906           MOVE IN73-KDTPD        TO UT-KDTPD(IX)                         
010907           ADD +1 TO IX                                                   
010908           MOVE JA TO SKRIV-SW                                            
010909        END-IF                                                            
010910     END-IF                                                               
011000     .                                                                    
011010     EJECT                                                                
011064 Z-FINIT SECTION.                                                         
011065                                                                          
011066     CLOSE INFIL                                                          
011070           W11426                                                         
011101                                                                          
011102     MOVE 'S' TO POSTSUM-OPKOD                                            
011110     CALL POSTSUM USING POSTSUM-PARM                                      
011200     .                                                                    
011301     EJECT                                                                
011302 S01-LAES-INFIL SECTION.                                                  
011303                                                                          
011304     READ INFIL INTO IN-AREA                                              
011305     AT END                                                               
011307        SET END-OF-INFIL TO TRUE                                          
011308                                                                          
011309     NOT AT END                                                           
011310        MOVE 'IN'         TO POSTSUM-FDNAMN                               
011311        MOVE 'W11426D1'   TO POSTSUM-DDNAMN2                              
011314        MOVE SPACE        TO POSTSUM-TRANSTYP                             
011315        CALL POSTSUM USING POSTSUM-PARM                                   
011316     END-READ                                                             
011317     .                                                                    
011318     EJECT                                                                
011402 S10-NOLLSTALL SECTION.                                                   
011403                                                                          
011404     MOVE ZERO  TO UT-IDARTNR                                             
011406     MOVE +1 TO IX                                                        
011407     PERFORM UNTIL IX > IX-MAX                                            
011408        MOVE SPACE TO UT-IDLEVNR(IX)                                      
011409                      UT-IDSTEKN(IX)                                      
011410                      UT-FLUPG(IX)                                        
011411                      UT-KDTPD(IX)                                        
011413        MOVE ZERO TO  UT-DAAAVV-UPG(IX)                                   
011414                      UT-DAAAVV-TPD(IX)                                   
011415        ADD +1 TO IX                                                      
011416     END-PERFORM                                                          
011417     .                                                                    
011418     EJECT                                                                
011419 S11-SKRIV-W11426 SECTION.                                                
011420                                                                          
011421     WRITE UT-POST FROM UT-AREA                                           
011423                                                                          
011424     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
011425     MOVE 'W11426'   TO POSTSUM-FDNAMN                                    
011426     MOVE 'W11426D2' TO POSTSUM-DDNAMN2                                   
011427     CALL POSTSUM USING POSTSUM-PARM                                      
011430     .                                                                    
011600     EJECT                                                                
