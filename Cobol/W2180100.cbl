000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2180100.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   92/03/25.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER INFIL MED ARTIKLAR DÄR TIDISPIN SKALL OMRÄKNAS,            
001100*        SORTERAR OCH TAR BORT DUBLETTER                                  
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
002502*          --- ARTIKLAR (ARTNR CLAGER) DÄR TIDISPIN SKALL OMRÄKNAS        
002503     SELECT INFIL                      ASSIGN TO W21801D1.                
002504     SKIP2                                                                
002505*          --- SORTERADE POSTER FÖR TIDISPIN BERÄKNING                    
002510     SELECT W21802                     ASSIGN TO W21801D2.                
002600     SKIP2                                                                
002700*          --- SORTERINGSFIL                                              
002800     SELECT SORTFIL                    ASSIGN TO W21801DS.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003401     SKIP3                                                                
003402 FD  INFIL                                                                
003403     RECORDING       F                                                    
003404     BLOCK CONTAINS  0.                                                   
003405     SKIP2                                                                
003406*01  -COPY W21801      -L.                                                
003407     SKIP3                                                                
003408 FD  W21802                                                               
003409     RECORDING       F                                                    
003410     BLOCK CONTAINS  0.                                                   
003411     SKIP2                                                                
003420*01  POST -COPY W21802 -PRE  UT-  -L.                                     
003500     SKIP3                                                                
003600 SD  SORTFIL.                                                             
003701     SKIP2                                                                
003702 01  SD-AREA.                                                             
003703     03  SD-RANDOMKEY         PIC X(4).                                   
003710*    03  POST -COPY W21801      -PRE SD-                                  
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000     SKIP2                                                                
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W2180100'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004501                                                                          
004502 77  DATABAS                     PIC X(4)    VALUE 'WDK6'.                
004503                                                                          
004506 77  WS-IDARTNR                  PIC S9(9) VALUE ZERO COMP-3.             
004507 77  INFIL-EOF-SW               PIC X       VALUE 'N'.                    
004510     88  END-OF-INFIL                       VALUE 'J'.                    
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
006300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006500     SKIP2                                                                
006600 01  FELTEXT.                                                             
006700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006901     EJECT                                                                
006902*    --- PARAMETRAR TILL POSTSUM                                          
006903*                                                                         
006910*01  -COPY W0005   -PRE  POSTSUM-                                         
007001     EJECT                                                                
007111                                                                          
007120*01  AREA -COPY W21802     -PRE UT-                                       
007201     EJECT                                                                
007202 01  UT-AREA-START           PIC X(24)   VALUE                            
007203                                  'UT-AREA-START  '.                      
007204     SKIP2                                                                
007205                                                                          
007206 01  SORT-RETURN-X            PIC X(2) VALUE SPACE.                       
007207                                                                          
007210     SKIP2                                                                
007400     EJECT                                                                
007500 PROCEDURE DIVISION.                                                      
007800                                                                          
007900     PERFORM A-INIT                                                       
008000                                                                          
008010     PERFORM S01-LAES-INFIL                                               
008020     IF NOT END-OF-INFIL                                                  
008100        SORT SORTFIL ASCENDING SD-RANDOMKEY SD-IDARTNR                    
008200                                                                          
008300                     INPUT PROCEDURE B-SORT-INPUT                         
008400                     OUTPUT PROCEDURE C-SORT-OUTPUT                       
008500                                                                          
008600        IF SORT-RETURN NOT = 0                                            
008700           MOVE SORT-RETURN TO SORT-RETURN-X                              
008800           STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                  
008900           DELIMITED BY SIZE INTO FELTEXT-STR                             
009000           DISPLAY 'SORTERINGSFEL'                                        
009100           PERFORM S99-ABEND                                              
009200        END-IF                                                            
009201     END-IF                                                               
009210                                                                          
009300     PERFORM Z-FINIT                                                      
009400                                                                          
009500     MOVE ZERO TO RETURN-CODE                                             
009600                                                                          
009800     GOBACK                                                               
009900     .                                                                    
010000     EJECT                                                                
010100 A-INIT SECTION.                                                          
010201                                                                          
010210     OPEN INPUT  INFIL                                                    
010301                                                                          
010310     OPEN OUTPUT W21802                                                   
010400     SKIP2                                                                
010500     ACCEPT DAGENS-DATUM  FROM DATE                                       
010610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010700     .                                                                    
010801     EJECT                                                                
010802 B-SORT-INPUT  SECTION.                                                   
010803     SKIP2                                                                
010805     PERFORM UNTIL END-OF-INFIL                                           
010808        CALL W015RAND USING SD-IDARTNR SD-RANDOMKEY DATABAS               
010809        RELEASE SD-AREA                                                   
010812        PERFORM S01-LAES-INFIL                                            
010813     END-PERFORM                                                          
010820     .                                                                    
010900     EJECT                                                                
011000 C-SORT-OUTPUT SECTION.                                                   
011100     SKIP2                                                                
011200     PERFORM S32-SORT-RETURN                                              
011210     MOVE SD-IDARTNR TO WS-IDARTNR                                        
011300     PERFORM UNTIL END-OF-SORTFIL                                         
011420        IF SD-IDARTNR NOT = WS-IDARTNR                                    
011421           MOVE WS-IDARTNR TO UT-IDARTNR                                  
011422           PERFORM S11-SKRIV-W21802                                       
011423           MOVE SD-IDARTNR TO WS-IDARTNR                                  
011424        END-IF                                                            
011430        PERFORM S32-SORT-RETURN                                           
011700     END-PERFORM                                                          
011710     MOVE WS-IDARTNR TO UT-IDARTNR                                        
011720     PERFORM S11-SKRIV-W21802                                             
011800     .                                                                    
011900     EJECT                                                                
012000 Z-FINIT SECTION.                                                         
012101     CLOSE INFIL                                                          
012110           W21802                                                         
012201     SKIP2                                                                
012202     MOVE 'S' TO POSTSUM-OPKOD                                            
012210     CALL POSTSUM USING POSTSUM-PARM                                      
012300     .                                                                    
012401     EJECT                                                                
012402 S01-LAES-INFIL   SECTION.                                                
012403     SKIP2                                                                
012404     READ INFIL  INTO SD-POST                                             
012405     AT END                                                               
012407        SET END-OF-INFIL  TO TRUE                                         
012408                                                                          
012409     NOT AT END                                                           
012410        MOVE 'INFIL' TO POSTSUM-FDNAMN                                    
012411        MOVE 'W21801D1' TO POSTSUM-DDNAMN2                                
012412        MOVE 'SPACE'   TO POSTSUM-TRANSTYP                                
012413        CALL POSTSUM USING POSTSUM-PARM                                   
012414     END-READ                                                             
012420     .                                                                    
012501     EJECT                                                                
012502 S11-SKRIV-W21802 SECTION.                                                
012503     SKIP2                                                                
012504     WRITE UT-POST FROM UT-AREA                                           
012505                                                                          
012506     MOVE 'SPACE'   TO POSTSUM-TRANSTYP                                   
012507     MOVE 'W21802' TO POSTSUM-FDNAMN                                      
012508     MOVE 'W21801D2' TO POSTSUM-DDNAMN2                                   
012509     CALL POSTSUM USING POSTSUM-PARM                                      
012510     .                                                                    
012700     EJECT                                                                
013300 S32-SORT-RETURN  SECTION.                                                
013400     SKIP2                                                                
013500     RETURN SORTFIL                                                       
013600     AT END                                                               
013700         SET END-OF-SORTFIL TO TRUE                                       
013800     .                                                                    
013900     EJECT                                                                
014000 S99-ABEND SECTION.                                                       
014100                                                                          
014201     SKIP2                                                                
014202     MOVE 'S' TO POSTSUM-OPKOD                                            
014210     CALL POSTSUM USING POSTSUM-PARM                                      
014300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
014400     .                                                                    
