000010                                                                          
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1117100.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   JUNI 1992.                                               
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER FIL W11170 MED ERSÄTTNINGS-TRANSAR              
001100*        OCH SKAPAR FIL W11171 TILL VR.                                   
001200*                                                                         
001300*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002501     SKIP2                                                                
002502*          --- ERSÄTTNINGS-TRANSAR FRÅN W111P070                          
002503     SELECT W11170                     ASSIGN TO W11171D1.                
002504     SKIP2                                                                
002505*          --- ERSÄTTNINGS-INFO TILL VR                                   
002510     SELECT W11171                     ASSIGN TO W11171D2.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003101     SKIP3                                                                
003102 FD  W11170                                                               
003103     RECORDING       V                                                    
003104     BLOCK CONTAINS  0.                                                   
003105     SKIP2                                                                
003106*01  -COPY W111701A      -L.                                              
003107     SKIP2                                                                
003108*01  -COPY W111702A      -L.                                              
003109     SKIP3                                                                
003110 FD  W11171                                                               
003111     RECORDING       V                                                    
003112     BLOCK CONTAINS  0.                                                   
003113     SKIP2                                                                
003114*01  POST -COPY W91020L1 -PRE  VR1-  -L.                                  
003115     SKIP2                                                                
003116*01  POST -COPY W91020L2 -PRE  VR2-  -L.                                  
003117     SKIP2                                                                
003120*01  POST -COPY W91020L3 -PRE  VR3-  -L.                                  
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003401                                                                          
003410*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)   VALUE 'W1117100'.             
003600 77  JA                          PIC X      VALUE 'J'.                    
003700 77  NEJ                         PIC X      VALUE 'N'.                    
003800 77  W-IDLOPNR                   PIC 9(5)   VALUE  ZERO   COMP-3.         
003901                                                                          
003902 01  WS-KDERS                    PIC 9(2)   VALUE ZERO.                   
003903 01  FILLER REDEFINES WS-KDERS.                                           
003904     03  WS-KDERS-1              PIC 9.                                   
003905     03  WS-KDERS-2              PIC 9.                                   
003906                                                                          
003907 77  W11170-EOF-SW               PIC X      VALUE 'N'.                    
003910     88  END-OF-W11170                      VALUE 'J'.                    
004000                                                                          
004010 01  KORNINGSDATUM               PIC 9(6).                                
004020                                                                          
004200 01  FILLER REDEFINES KORNINGSDATUM.                                      
004300     03  AA                      PIC 9(2).                                
004400     03  MM                      PIC 9(2).                                
004500     03  DD                      PIC 9(2).                                
004600     EJECT                                                                
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800*                                                                         
005010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005020     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006101     EJECT                                                                
006102 01  PARAM-TILL-DATUMKORT.                                                
006103     03  PROG-ID                 PIC X(8)   VALUE 'W1117100'.             
006104     03  KORT-ID                 PIC X(6)   VALUE 'WDATUM'.               
006714*01  -COPY WDATKORT.                                                      
006715     EJECT                                                                
006716 01  IN-AREA-START               PIC X(24)   VALUE                        
006717                                 'IN-AREA-START  '.                       
006718 01  IN-AREA.                                                             
006719     03  IN-IDPTYP               PIC X(3).                                
006720     03  FILLER                  PIC X(150).                              
006721*01  FILLER -COPY W111701A      -PRE IN701-   -RED  IN-AREA               
006722     EJECT                                                                
006723*01  FILLER -COPY W111702A      -PRE IN702-   -RED  IN-AREA               
006724     EJECT                                                                
006725 01  VR1-AREA-START              PIC X(24)   VALUE                        
006726                                 'VR1-AREA-START '.                       
006727                                                                          
006728*01  AREA   -COPY W91020L1      -PRE VR1-                                 
006729     EJECT                                                                
006730 01  VR2-AREA-START              PIC X(24)   VALUE                        
006731                                 'VR2-AREA-START '.                       
006732                                                                          
006733*01  AREA   -COPY W91020L2      -PRE VR2-                                 
006734     EJECT                                                                
006735 01  VR3-AREA-START              PIC X(24)   VALUE                        
006736                                 'VR3-AREA-START '.                       
006737                                                                          
006738*01  AREA   -COPY W91020L3      -PRE VR3-                                 
006739     EJECT                                                                
006740 PROCEDURE DIVISION.                                                      
006741                                                                          
006750                                                                          
006800     PERFORM A-INIT                                                       
006900                                                                          
006910     PERFORM S01-LAES-W11170                                              
007000     PERFORM UNTIL END-OF-W11170                                          
007010                                                                          
007100        EVALUATE TRUE                                                     
007200           WHEN IN-IDPTYP = '701'                                         
007300              PERFORM B-KOLLA-SKAPA-VR1-VR2-POST                          
007400           WHEN IN-IDPTYP = '702'                                         
007500              PERFORM C-SKAPA-VR3-POST                                    
007600        END-EVALUATE                                                      
007700                                                                          
007710        PERFORM S01-LAES-W11170                                           
007800     END-PERFORM                                                          
008000                                                                          
008100     PERFORM Z-FINIT                                                      
008300     MOVE ZERO TO RETURN-CODE                                             
008400     GOBACK                                                               
008500     .                                                                    
008600     EJECT                                                                
008700 A-INIT SECTION.                                                          
008801                                                                          
008810     OPEN INPUT  W11170                                                   
008910     OPEN OUTPUT W11171                                                   
009000                                                                          
009010     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
009020                                                                          
009100     MOVE D-AAR      TO AA                                                
009200     MOVE D-MAANAD   TO MM                                                
009210     MOVE D-DAG      TO DD                                                
009301                                                                          
009302     MOVE ZERO       TO W-IDLOPNR                                         
009310     .                                                                    
009400     EJECT                                                                
009401 B-KOLLA-SKAPA-VR1-VR2-POST SECTION.                                      
009402                                                                          
009405*****************************************************************         
009406*  VR1-POST = BACKNING                                          *         
009408*  - ÄT FEB 93  SAMTLIGA RIVNINGAR TILL 00                      *         
009409*               OBEROENDE AV TIDIGARE EK                        *         
009410*  - NÄR GAMMAL EK > 20 OCH NY EK > 20                          *         
009411*      ERSÄTTNINGEN RIVS DÅ OCH LÄGGS UPP PÅ NYTT               *         
009412*    - OM OLIKA EK = BYTE AV EK                                 *         
009413*    - OM SAMMA EK = UPPDATERING AV TILLKOMMANDE ARTIKLAR       *         
009414*                                                               *         
009415*  VR2-POST = ERSÄTTNING (ERSATT ARTIKEL)                       *         
009416*  - NÄR ERSÄTTNINGEN GÅR UPPÅT TILL > 10                       *         
009417*  - NÄR GAMMAL EK > 20 OCH NY EK > 20                          *         
009418*****************************************************************         
009419                                                                          
009420     IF IN701-KDERS-OLD > IN701-KDERS-NEW OR                              
009421        (IN701-KDERS-OLD > 20 AND IN701-KDERS-NEW > 20)                   
009422        MOVE '1'                   TO VR1-IDPOST                          
009423        MOVE KORNINGSDATUM         TO VR1-TIAAMMDD                        
009430        MOVE IN701-IDARTNR-ERS     TO VR1-IDARTNR-ERS                     
009431        MOVE IN701-KDERS-OLD       TO VR1-KDERS-GAMMAL                    
009432        IF IN701-KDERS-OLD > 20 AND IN701-KDERS-NEW > 20                  
009433           MOVE ZERO               TO VR1-KDERS-NY                        
009434        ELSE                                                              
009436           MOVE IN701-KDERS-NEW    TO WS-KDERS                            
009437           IF WS-KDERS-1 = 0                                              
009438              MOVE ZERO            TO VR1-KDERS-NY                        
009439           ELSE                                                           
009440              MOVE IN701-KDERS-NEW TO VR1-KDERS-NY                        
009450           END-IF                                                         
009453        END-IF                                                            
009460        ADD +1 TO W-IDLOPNR                                               
009470        MOVE W-IDLOPNR             TO VR1-IDLOPNR                         
009480                                                                          
009481        IF VR1-KDERS-NY = ZERO                                            
009490           PERFORM S11-SKRIV-VR1-POST                                     
009491        ELSE                                                              
009493           MOVE SPACE TO VR1-AREA                                         
009494           ADD -1 TO W-IDLOPNR                                            
009495        END-IF                                                            
009496     END-IF                                                               
009497                                                                          
009498     IF IN701-KDERS-OLD < IN701-KDERS-NEW OR                              
009499       (IN701-KDERS-OLD > 20 AND IN701-KDERS-NEW > 20)                    
009500                                                                          
009501         MOVE '2'                TO VR2-IDPOST                            
009502         MOVE KORNINGSDATUM      TO VR2-TIAAMMDD                          
009503         MOVE IN701-IDARTNR-ERS  TO VR2-IDARTNR-ERS                       
009504         MOVE IN701-REKSIFFR-ERS TO VR2-REKSIFFR-ERS                      
009505         MOVE IN701-KDERS-NEW    TO VR2-KDERS                             
009506         MOVE IN701-DIERS-ERS    TO VR2-DIERS-ERS                         
009507         MOVE IN701-TIERSDAT     TO VR2-TIERSDAT                          
009508         MOVE IN701-BEART-SVE    TO VR2-BEART-SVE                         
009509         MOVE IN701-BEART-ENG    TO VR2-BEART-ENG                         
009510         MOVE IN701-BEART-TYS    TO VR2-BEART-TYS                         
009511         MOVE IN701-BEART-SPA    TO VR2-BEART-SPA                         
009520         MOVE IN701-BEART-FRA    TO VR2-BEART-FRA                         
009537                                                                          
009538         ADD +1 TO W-IDLOPNR                                              
009540         MOVE W-IDLOPNR          TO VR2-IDLOPNR                           
009541                                                                          
009542         PERFORM S12-SKRIV-VR2-POST                                       
009543     END-IF                                                               
009544     .                                                                    
009545     EJECT                                                                
009550 C-SKAPA-VR3-POST SECTION.                                                
009551                                                                          
009552*****************************************************************         
009553*  VR3-POST = ERSÄTTNING (TILLKOMMANDE ARTIKEL)                 *         
009561*  - NÄR ERSÄTTNINGEN GÅR UPPÅT TILL > 10                       *         
009562*  - NÄR GAMMAL EK > 20 OCH NY EK > 20                          *         
009563*****************************************************************         
009564                                                                          
009565     MOVE '3'                      TO VR3-IDPOST                          
009566     MOVE KORNINGSDATUM            TO VR3-TIAAMMDD                        
009567     MOVE IN702-IDARTNR-ERS        TO VR3-IDARTNR-ERS                     
009569     MOVE IN702-IDKORTNR           TO VR3-IDKORTNR                        
009570     MOVE IN702-FLTEXT             TO VR3-FLTEXT                          
009571     IF IN702-FLTEXT = 'J'                                                
009572        MOVE IN702-BEERS           TO VR3-BEERS                           
009573     ELSE                                                                 
009574        MOVE IN702-IDARTNR-TILLK   TO VR3-IDARTNR-TILLK                   
009575        MOVE IN702-REKSIFFR-TILLK  TO VR3-REKSIFFR-TILLK                  
009576        MOVE IN702-DIERS-TILLK     TO VR3-DIERS-TILLK                     
009577        MOVE IN702-BEART-SVE-TILLK TO VR3-BEART-SVE-TILLK                 
009578        MOVE IN702-BEART-ENG-TILLK TO VR3-BEART-ENG-TILLK                 
009579        MOVE IN702-BEART-TYS-TILLK TO VR3-BEART-TYS-TILLK                 
009580        MOVE IN702-BEART-SPA-TILLK TO VR3-BEART-SPA-TILLK                 
009581        MOVE IN702-BEART-FRA-TILLK TO VR3-BEART-FRA-TILLK                 
009582     END-IF                                                               
009590                                                                          
009613     ADD +1 TO W-IDLOPNR                                                  
009614     MOVE W-IDLOPNR                TO VR3-IDLOPNR                         
009615                                                                          
009616     PERFORM S13-SKRIV-VR3-POST                                           
009617     .                                                                    
009618     EJECT                                                                
009624 Z-FINIT SECTION.                                                         
009625                                                                          
009626     CLOSE W11170                                                         
009630           W11171                                                         
009701                                                                          
009702     MOVE 'S' TO POSTSUM-OPKOD                                            
009710     CALL POSTSUM USING POSTSUM-PARM                                      
009800     .                                                                    
009901     EJECT                                                                
009902 S01-LAES-W11170  SECTION.                                                
009903     SKIP2                                                                
009904     READ W11170 INTO IN-AREA                                             
009905     AT END                                                               
009907        SET END-OF-W11170 TO TRUE                                         
009908                                                                          
009909     NOT AT END                                                           
009910        MOVE 'W11170'   TO POSTSUM-FDNAMN                                 
009911        MOVE 'W11171D1' TO POSTSUM-DDNAMN2                                
009912        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
009913        CALL POSTSUM  USING POSTSUM-PARM                                  
009914     END-READ                                                             
009920     .                                                                    
010001     EJECT                                                                
010002 S11-SKRIV-VR1-POST SECTION.                                              
010003                                                                          
010004     WRITE VR1-POST FROM VR1-AREA                                         
010005                                                                          
010006     MOVE 'VR1'      TO POSTSUM-TRANSTYP                                  
010007     MOVE 'W11171'   TO POSTSUM-FDNAMN                                    
010008     MOVE 'W11171D2' TO POSTSUM-DDNAMN2                                   
010009     CALL POSTSUM USING POSTSUM-PARM                                      
010010                                                                          
010011     MOVE SPACE TO VR1-AREA                                               
010012     .                                                                    
010013     EJECT                                                                
010020 S12-SKRIV-VR2-POST SECTION.                                              
010030                                                                          
010040     WRITE VR2-POST FROM VR2-AREA                                         
010050                                                                          
010060     MOVE 'VR2'      TO POSTSUM-TRANSTYP                                  
010070     MOVE 'W11171'   TO POSTSUM-FDNAMN                                    
010080     MOVE 'W11171D2' TO POSTSUM-DDNAMN2                                   
010090     CALL POSTSUM USING POSTSUM-PARM                                      
010100                                                                          
010102     MOVE SPACE TO VR2-AREA                                               
010103     .                                                                    
010110     EJECT                                                                
010200 S13-SKRIV-VR3-POST SECTION.                                              
010300                                                                          
010400     WRITE VR3-POST FROM VR3-AREA                                         
010500                                                                          
010600     MOVE 'VR3'      TO POSTSUM-TRANSTYP                                  
010700     MOVE 'W11171'   TO POSTSUM-FDNAMN                                    
010800     MOVE 'W11171D2' TO POSTSUM-DDNAMN2                                   
010900     CALL POSTSUM USING POSTSUM-PARM                                      
010910                                                                          
010920     MOVE SPACE TO VR3-AREA                                               
011000     .                                                                    
011100     EJECT                                                                
