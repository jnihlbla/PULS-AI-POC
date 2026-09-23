000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W1117700.                                                
000004*AUTHOR.         BODIL LINDAHL.                                           
000005*DATE-WRITTEN.   SEPTEMBER 2000.                                          
000006*DATE-COMPILED.                                                           
000007*                                                                         
000008*    FUNKTION:                                                            
000009*        PROGRAMMET LÄSER FIL W11170 MED ERSÄTTNINGS-TRANSAR              
000010*        OCH SKAPAR FIL W11177 TILL STICS.                                
000020*                                                                         
000030*                                                                         
000040                                                                          
000050     SKIP3                                                                
000060 ENVIRONMENT DIVISION.                                                    
000070     SKIP2                                                                
000080 INPUT-OUTPUT SECTION.                                                    
000090                                                                          
000100 FILE-CONTROL.                                                            
000200     SKIP2                                                                
000300*          --- ERSÄTTNINGS-TRANSAR FRÅN W111P070                          
000400     SELECT W11170                     ASSIGN TO W11177D1.                
000500     SKIP2                                                                
000600*          --- ERSÄTTNINGS-INFO TILL STICS                                
000700     SELECT W11177                     ASSIGN TO W11177D2.                
000800     EJECT                                                                
000900 DATA DIVISION.                                                           
001000     SKIP3                                                                
001100 FILE SECTION.                                                            
001200     SKIP3                                                                
001300 FD  W11170                                                               
001400     RECORDING       V                                                    
001500     BLOCK CONTAINS  0.                                                   
001600     SKIP2                                                                
001700*01  -COPY W111701A      -L.                                              
001800     SKIP2                                                                
001900*01  -COPY W111702A      -L.                                              
002000     SKIP3                                                                
002100 FD  W11177                                                               
002200     RECORDING       F                                                    
002300     BLOCK CONTAINS  0.                                                   
002400*01  POST  -COPY W11177  -PRE UT-  -L.                                    
003110     EJECT                                                                
003120 WORKING-STORAGE SECTION.                                                 
003130     SKIP2                                                                
003140                                                                          
003141*    -- CHECKED BY WY2000                                                 
003142 77  IDPGM                       PIC X(8)   VALUE 'W1117700'.             
003143 77  JA                          PIC X      VALUE 'J'.                    
003144 77  NEJ                         PIC X      VALUE 'N'.                    
003147                                                                          
003148 77  WS-IDARTNR-ERS              PIC S9(9)  VALUE  ZERO   COMP-3.         
003150 77  WS-REKSIFFR-ERS             PIC S9     VALUE  ZERO   COMP-3.         
003151 77  WS-TIERSDAT                 PIC S9(5)  VALUE  ZERO   COMP-3.         
003152 77  WS-DIERS-ERS                PIC S9(4)V9(3) VALUE ZERO                
003160                                                          COMP-3.         
003181 77  WS-KDERS-NEW                PIC 9(3).                                
003182     88 ENTYDIG-ERSATTNING          VALUE 21 22 23 27.                    
003183     88 EJ-ENTYDIG-ERSATTNING       VALUE 24 25 26 28.                    
003184                                                                          
003185 77  W11170-EOF-SW               PIC X      VALUE 'N'.                    
003186     88  END-OF-W11170                      VALUE 'J'.                    
003187                                                                          
003188 77  BEHANDLA-702-SW             PIC X      VALUE 'N'.                    
003189     88  BEHANDLA-702                       VALUE 'J'.                    
003190                                                                          
003191 01  WS-TIME.                                                             
003192     03  WS-TIME-HHMMSS          PIC 9(6).                                
003193     03  FILLER                  PIC 9(2).                                
003200                                                                          
003600 01  DYNAMISKA-SUBPROGRAM.                                                
003700*                                                                         
003800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
003900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
004000     EJECT                                                                
004100*    --- PARAMETRAR TILL POSTSUM                                          
004200*                                                                         
004300*01  -COPY W0005   -PRE  POSTSUM-                                         
004400     EJECT                                                                
004500*    --- PARAMETRAR TILL DATUMKORT                                        
004600*                                                                         
004700 01  PROGRAM-NAMN               PIC X(6)     VALUE 'W11177'.              
004800 01  DATUMKORT-ID               PIC X(6)     VALUE 'WDATUM'.              
004900                                                                          
005000*01  -COPY WDATKORT                                                       
005100     EJECT                                                                
005200 01  IN-AREA-START               PIC X(24)   VALUE                        
005300                                 'IN-AREA-START  '.                       
005400 01  IN-AREA.                                                             
005500     03  IN-IDPTYP               PIC X(3).                                
005600     03  FILLER                  PIC X(150).                              
005700*01  FILLER -COPY W111701A      -PRE IN701-   -RED  IN-AREA               
005800     EJECT                                                                
005900*01  FILLER -COPY W111702A      -PRE IN702-   -RED  IN-AREA               
006000     EJECT                                                                
006010 01  UT-AREA-START               PIC X(24)   VALUE                        
006020                                 'UT-AREA-START  '.                       
006110*01  AREA   -COPY W11177        -PRE UT-                                  
006740     EJECT                                                                
006741 PROCEDURE DIVISION.                                                      
006742                                                                          
006743                                                                          
006744     PERFORM A-INIT                                                       
006745                                                                          
006746     PERFORM S01-LAES-W11170                                              
006747     PERFORM UNTIL END-OF-W11170                                          
006748                                                                          
006749        EVALUATE TRUE                                                     
006750           WHEN IN-IDPTYP = '701'                                         
006752              IF IN701-KDERS-NEW = 21 OR 22 OR 23                         
006753                 MOVE IN701-IDARTNR-ERS                                   
006754                                  TO WS-IDARTNR-ERS                       
006755                 MOVE IN701-REKSIFFR-ERS                                  
006756                                  TO WS-REKSIFFR-ERS                      
006757                 MOVE IN701-KDERS-NEW                                     
006758                                  TO WS-KDERS-NEW                         
006759                 MOVE IN701-DIERS-ERS                                     
006760                                  TO WS-DIERS-ERS                         
006761                 MOVE IN701-TIERSDAT                                      
006762                                  TO WS-TIERSDAT                          
006763                 MOVE JA  TO BEHANDLA-702-SW                              
006770              ELSE                                                        
006771                 MOVE NEJ TO BEHANDLA-702-SW                              
006772              END-IF                                                      
006773           WHEN IN-IDPTYP = '702'                                         
006774              IF BEHANDLA-702                                             
006775                 IF IN702-IDARTNR-ERS = WS-IDARTNR-ERS                    
006776                    PERFORM C-SKAPA-UTPOST-MED-TILLK                      
006777                 END-IF                                                   
006778              END-IF                                                      
006779        END-EVALUATE                                                      
006780                                                                          
006781        PERFORM S01-LAES-W11170                                           
006782     END-PERFORM                                                          
006783                                                                          
006784     PERFORM Z-FINIT                                                      
006785     MOVE ZERO TO RETURN-CODE                                             
006790     GOBACK                                                               
006800     .                                                                    
006900     EJECT                                                                
007000 A-INIT SECTION.                                                          
007100                                                                          
007200     OPEN INPUT  W11170                                                   
007300     OPEN OUTPUT W11177                                                   
007400                                                                          
007500     MOVE FUNCTION CURRENT-DATE(1:8) TO UT-DADATUM                        
007600     ACCEPT WS-TIME FROM TIME                                             
007700     MOVE WS-TIME-HHMMSS        TO UT-TIHHMMSS                            
007800     MOVE 'QP8'                 TO UT-IDPTYP                              
007810     MOVE 'W111Z2SE '           TO UT-PARTNER-ID                          
007900     .                                                                    
008000     EJECT                                                                
009500 C-SKAPA-UTPOST-MED-TILLK SECTION.                                        
009501                                                                          
009502     MOVE WS-IDARTNR-ERS  TO UT-IDARTNR-ERS                               
009503     MOVE WS-REKSIFFR-ERS TO UT-REKSIFFR-ERS                              
009504     MOVE WS-DIERS-ERS    TO UT-DIERS-ERS                                 
009505     MOVE WS-TIERSDAT     TO UT-TIERSDAT                                  
009508     MOVE WS-KDERS-NEW    TO UT-KDERS                                     
009509     MOVE IN702-IDKORTNR  TO UT-IDKORTNR                                  
009510     MOVE IN702-FLTEXT    TO UT-FLTEXT                                    
009512     MOVE IN702-IDARTNR-TILLK  TO UT-IDARTNR-TILLK                        
009513     MOVE IN702-REKSIFFR-TILLK TO UT-REKSIFFR-TILLK                       
009514     MOVE IN702-DIERS-TILLK    TO UT-DIERS-TILLK                          
009518                                                                          
009519     PERFORM S10-SKRIV-UTPOST                                             
009695     .                                                                    
009696     EJECT                                                                
009743 Z-FINIT SECTION.                                                         
009744                                                                          
009745     CLOSE W11170                                                         
009746           W11177                                                         
009747                                                                          
009748     MOVE 'S' TO POSTSUM-OPKOD                                            
009749     CALL POSTSUM USING POSTSUM-PARM                                      
009750     .                                                                    
009751     EJECT                                                                
009752 S01-LAES-W11170  SECTION.                                                
009753                                                                          
009754     READ W11170 INTO IN-AREA                                             
009755     AT END                                                               
009756        SET END-OF-W11170 TO TRUE                                         
009757                                                                          
009758     NOT AT END                                                           
009759        MOVE 'W11170'   TO POSTSUM-FDNAMN                                 
009760        MOVE 'W11177D1' TO POSTSUM-DDNAMN2                                
009770        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
009780        CALL POSTSUM  USING POSTSUM-PARM                                  
009790     END-READ                                                             
009800     .                                                                    
009900     EJECT                                                                
009910 S10-SKRIV-UTPOST SECTION.                                                
009911                                                                          
009912     WRITE UT-POST FROM UT-AREA                                           
009913                                                                          
009914     MOVE 'W11177'   TO POSTSUM-FDNAMN                                    
009915     MOVE 'W11177D2' TO POSTSUM-DDNAMN2                                   
009916     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
009917     CALL POSTSUM USING POSTSUM-PARM                                      
009918     .                                                                    
009919     EJECT                                                                
