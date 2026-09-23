000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W510VTH.                                                 
000500 AUTHOR.        KARL JOHAN HANSSON.                                       
000600 DATE-WRITTEN.  JANUARI 1996.                                             
000700*    REMARKS.                                                             
000800*                                                                         
000900*        ANROP:          SKER GENOM CALL W510VTH                          
001000*                                                                         
001100*        BEHANDLING      DENNA MODUL BESTÄMMER KDVTH MED                  
001200*                        HJÄLP AV SUBMODUL W611STYR.                      
001300*                                                                         
001500*        LÄNKAREA        W510VTH                                          
001600*                                                                         
001700*        ANROP SKER MED  IDDC, IDARTNR, IDLEVNR, IDFKNGRP, BEFT           
001900*                                                                         
002000*        SVAR FRÅN SUBPROGRAMMET GER KDVTH  1 - 9                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 DATA DIVISION.                                                           
002500     SKIP2                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM              PIC X(8)     VALUE 'W510VTH'.                     
003100                                                                          
003200 01  DYNAMISKA-SUBPROGRAM.                                                
003300   03  W611STYR              PIC X(8)    VALUE 'W611STYR'.                
003400     SKIP2                                                                
003500*    -COPY W611STYR                                                       
003510                                                                          
003520*    -- VALID IDDC CODES                                                  
003530*01  -COPY WWDCKONS                                                       
003600     EJECT                                                                
003700 LINKAGE SECTION.                                                         
003800                                                                          
003900 01  VTH-AREA.                                                            
004000     03  -COPY W510VTH                                                    
004100     EJECT                                                                
004200*01  -COPY W0008      -PRE  HANB-                                         
004300       05  FILLER                PIC X.                                   
004400                                                                          
004500*01  -COPY W0008      -PRE  PLAA-                                         
004600       05  FILLER                PIC X.                                   
004700     EJECT                                                                
004800 PROCEDURE DIVISION  USING VTH-AREA HANB-PCB PLAA-PCB.                    
005000                                                                          
005100 STYR SECTION.                                                            
005110                                                                          
005120     MOVE +9                           TO VTH-KDVTH                       
005200                                                                          
005300     MOVE VTH-IDDC                     TO STYR-IDDC                       
005400     MOVE VTH-IDARTNR                  TO STYR-IDARTNR                    
005500     MOVE VTH-IDFKNGRP                 TO STYR-IDFKNGRP                   
005600     MOVE VTH-IDLEVNR                  TO STYR-IDLEVNR                    
005700     MOVE VTH-BEFT                     TO STYR-BEFT                       
006000     CALL W611STYR USING STYR-W611STYR                                    
006100                         HANB-PCB PLAA-PCB                                
006400     IF STYR-KDSVAR-OK                                                    
006500       EVALUATE STYR-ADINLOMR-FP                                          
006600         WHEN 'MÅLU'         MOVE +1   TO VTH-KDVTH                       
006700         WHEN 'F10 '         MOVE +2   TO VTH-KDVTH                       
006800         WHEN 'F2  '         MOVE +3   TO VTH-KDVTH                       
006900         WHEN OTHER                                                       
007000           EVALUATE STYR-ADINLOMR-FB                                      
007100              WHEN '573 '    MOVE +5   TO VTH-KDVTH                       
007110              WHEN 'FB1 '    MOVE +5   TO VTH-KDVTH                       
007200              WHEN 'FB2 '    MOVE +5   TO VTH-KDVTH                       
007300              WHEN 'FB3 '    MOVE +6   TO VTH-KDVTH                       
007400              WHEN 'FB4 '    MOVE +6   TO VTH-KDVTH                       
007500              WHEN 'FBR '    MOVE +6   TO VTH-KDVTH                       
007600              WHEN 'FB5 '    MOVE +7   TO VTH-KDVTH                       
007700              WHEN 'FB6 '    MOVE +7   TO VTH-KDVTH                       
007800              WHEN 'FB? '                                                 
007900                MOVE WC-CDC-TR         TO STYR-IDDC                       
008000                CALL W611STYR USING STYR-W611STYR                         
008100                              HANB-PCB PLAA-PCB                           
008200                IF STYR-ADINLOMR-FB = 'INSP'                              
008300                   MOVE +4             TO VTH-KDVTH                       
008400                END-IF                                                    
008500           END-EVALUATE                                                   
008600       END-EVALUATE                                                       
008700     END-IF                                                               
008800                                                                          
008900     MOVE ZERO TO RETURN-CODE                                             
009000     GOBACK                                                               
009100     .                                                                    
