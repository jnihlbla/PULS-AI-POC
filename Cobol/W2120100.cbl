000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W2120100.                                                
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 18:37:52.                         
000800*AUTHOR.         TOMAS SVENSSON.                                          
000900*DATE-WRITTEN.   NOV 1978.                                                
001000*REMARKS.                                                                 
001100*        FUNKTION.                                                        
001200*                PROGRAMMET SKRIVER KONTROLLERADE                         
001300*                OCH REDIGERADE TRANSAKTIONER PÅ                          
001400*                TRANSFILEN W09222.                                       
001500*        INDATA.                                                          
001600*                TRANSAKTIONSTYPER   R22 OCH R23                          
001700*        UTDATA.                                                          
001800*                TRANSFIL W09222.    DDNAMN  W09206DD.                    
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     SELECT  W09222  ASSIGN TO UT-S-W09206DD.                             
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W09222                                                               
002900     RECORDING F                                                          
003000     BLOCK CONTAINS 0 RECORDS.                                            
003100     SKIP2                                                                
003200*01  W212R22 -COPY W212R22 -PRE UT- -L.                                   
003400     SKIP2                                                                
003500*01  W212R23 -COPY W212R23 -PRE UT- -L.                                   
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004510                                                                          
004511                                                                          
004512*    -- CHECKED BY WY2000                                                 
004520 01  FL-FIRST                PIC X(1) VALUE 'J'.                          
004600 01  INDX                    PIC S9(4) COMP SYNC VALUE +0.                
004700     EJECT                                                                
004800 LINKAGE SECTION.                                                         
004900     SKIP2                                                                
005000 01  TRANS-PARM.                                                          
005100     03  KOD                 PIC X(3).                                    
005200     03  FELTAB OCCURS 100.                                               
005300         05  FELKOD          PIC X(3).                                    
005400         05  FELTEXT         PIC X(15).                                   
005500     03  FILLER              PIC X(3).                                    
005600     03  INPOST              PIC X(100).                                  
005700*    03  FILLER -COPY W212R22K -PRE IN- -RED INPOST.                      
005900     03  UTAREA.                                                          
006000         05  FILLER          PIC X(36).                                   
006100         05  UTPOST          PIC X(214).                                  
006200*        05  FILLER -COPY W212R22 -PRE R22- -RED UTPOST.                  
006400*        05  FILLER -COPY W212R23 -PRE R23- -RED UTPOST.                  
006600     EJECT                                                                
006700 PROCEDURE DIVISION USING TRANS-PARM.                                     
006900     SKIP2                                                                
007000******************************************************************        
007100*    PROGRAMMET ANROPAS DYNAMISKT AV W09206.                     *        
007200*    TRANSTYP R22 OCH R23 SKRIVS PÅ TRANSFIL W09222.             *        
007300******************************************************************        
007400     SKIP2                                                                
007500 STYR SECTION.                                                            
007600     SKIP2                                                                
007700     IF FL-FIRST = 'J'                                                    
007710        OPEN OUTPUT W09222                                                
007800        MOVE 'N' TO FL-FIRST                                              
007810     END-IF                                                               
008000     IF KOD = 'EOF'                                                       
008100       CLOSE W09222                                                       
008200       GOBACK                                                             
008300     END-IF                                                               
008400     SKIP2                                                                
008500     IF R22-IDPTYP = 'R22'                                                
008600       PERFORM A-KOLLA-R22                                                
008700     ELSE                                                                 
008800       IF R23-IDPTYP = 'R23'                                              
008900         PERFORM S20-SKRIV-R23                                            
009000       END-IF                                                             
009100     END-IF                                                               
009200     GOBACK                                                               
009300     .                                                                    
009700     EJECT                                                                
009800 A-KOLLA-R22 SECTION.                                                     
009900     SKIP2                                                                
010000     MOVE +1 TO INDX                                                      
010100     IF IN-KDBEH-BEST = '3' OR '4'                                        
010200       IF IN-IDBEST = SPACE                                               
010300         CONTINUE                                                         
010400       ELSE                                                               
010500         MOVE '101' TO FELKOD (INDX)                                      
010600         MOVE SPACE TO FELTEXT (INDX)                                     
010700         ADD +1 TO INDX                                                   
010800       END-IF                                                             
010900     ELSE                                                                 
011000       IF IN-KDBEH-BEST = '1' OR '2' OR '5' OR '6'                        
011100         IF IN-IDBEST > '000000000000'                                    
011200           CONTINUE                                                       
011300         ELSE                                                             
011400           MOVE '102' TO FELKOD (INDX)                                    
011500           MOVE SPACE TO FELTEXT (INDX)                                   
011600           ADD +1 TO INDX                                                 
011700         END-IF                                                           
011800       END-IF                                                             
011900     END-IF                                                               
012000     IF FELKOD (1) = HIGH-VALUE                                           
012100       PERFORM S10-SKRIV-R22                                              
012200     ELSE                                                                 
012300       MOVE HIGH-VALUE TO FELKOD (INDX)                                   
012400     END-IF                                                               
012500     CONTINUE.                                                            
012600     EJECT                                                                
012700 S10-SKRIV-R22 SECTION.                                                   
012800     SKIP2                                                                
012900     WRITE UT-W212R22 FROM R22-W212R22                                    
013000     CONTINUE.                                                            
013100     EJECT                                                                
013200 S20-SKRIV-R23 SECTION.                                                   
013300     SKIP2                                                                
013400     WRITE UT-W212R23 FROM R23-W212R23                                    
013500     CONTINUE.                                                            
