000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1168000.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   00/01/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER VCOM-TRANS FRÅN BESTÄLLARE OCH STARTAR                     
001000*        RUTIN ERS.INFO TOT/ART.INFO TOT TILL IMPORTÖR/NDC.               
001100*                                                                         
001200*                                                                         
001210*                                                                         
001220* 2011-12-07  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1                
001221*                                                                         
001222* 2012-04-16  E-TRACKER 10169250 CN-W EXTRA INSTALLATIONER                
001230*                                                                         
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
002400     SKIP2                                                                
002500*          --- VCOM-ID FRÅN BESTÄLLARE                                    
002600     SELECT SYSIN                      ASSIGN TO W11680D1.                
002700     SKIP2                                                                
002800*          --- INFIL FRÅN BESTÄLLARE                                      
002900     SELECT INFIL                      ASSIGN TO W11680D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  SYSIN                                                                
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900 01  PARM             PIC X(80).                                          
004000     SKIP3                                                                
004100 FD  INFIL                                                                
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500 01  IN-POST          PIC X(80).                                          
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W1168000'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  SYSIN-EOF-SW                PIC X       VALUE 'N'.                   
005600     88  END-OF-SYSIN                        VALUE 'J'.                   
005700                                                                          
005800 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
005900     88  END-OF-INFIL                        VALUE 'J'.                   
006000                                                                          
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
006300     03  W980SOP                 PIC X(8)    VALUE 'W980SOP '.            
006400     EJECT                                                                
006500*    ---- PARAMETRAR TILL W980SOP                                         
006600*01  -COPY WSOPAREA.                                                      
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL ABEND                                            
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  FELTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800 01  PARM-AREA-START             PIC X(24)   VALUE                        
007900                                 'PARM-AREA-START'.                       
008000 01  PARM-AREA.                                                           
008100     03  FILLER                  PIC X(6).                                
008200     03  PARM-VCOM               PIC X(2).                                
008300                                                                          
008400 01  SOP-SYMB-AREA.                                                       
008500     03  FILLER                  PIC X(5)  VALUE 'VCOM('.                 
008600     03  SOP-EXP                 PIC X(8).                                
008700     03  FILLER                  PIC X     VALUE ')'.                     
008710     03  FILLER                  PIC X     VALUE SPACE.                   
008810     03  FILLER                  PIC X(10) VALUE 'COUNTRYX2('.            
008820     03  SOP-IDLANDX2            PIC X(2).                                
008830     03  FILLER                  PIC X     VALUE ')'.                     
008840                                                                          
008900 01  IN-AREA-START               PIC X(24)   VALUE                        
009000                                 'IN-AREA-START  '.                       
009100 01  IN-AREA.                                                             
009200     03  IN-SENDERTAG            PIC X(8).                                
009201     03  FILLER                  PIC X(26).                               
009210     03  IN-BESTALLNING          PIC X(5).                                
009300     03  FILLER                  PIC X(41).                               
009400     EJECT                                                                
009500 PROCEDURE DIVISION.                                                      
009600 MAIN SECTION.                                                            
009700                                                                          
009800     PERFORM A-INIT                                                       
009900                                                                          
010000     PERFORM S01-LAES-SYSIN                                               
010100     MOVE PARM-AREA TO SOP-EXP                                            
010101     IF PARM-VCOM = 'C1'                                                  
010102        MOVE 'CN'      TO SOP-IDLANDX2                                    
010103     ELSE                                                                 
010104        MOVE PARM-VCOM TO SOP-IDLANDX2                                    
010105     END-IF                                                               
010200     PERFORM S02-LAES-INFIL                                               
010210     DISPLAY 'IN-AREA=' IN-AREA                                           
010300     PERFORM B-KOLLA-STARTA-SOP                                           
010400                                                                          
010500     PERFORM Z-FINIT                                                      
010600     MOVE ZERO TO RETURN-CODE                                             
010700     GOBACK                                                               
010800     .                                                                    
010900     EJECT                                                                
011000 A-INIT SECTION.                                                          
011100                                                                          
011200     OPEN INPUT SYSIN                                                     
011300                INFIL                                                     
011400     .                                                                    
011500     EJECT                                                                
011600 B-KOLLA-STARTA-SOP SECTION.                                              
011700                                                                          
011800************* TOTALFIL ARTIKELINFO                                        
011900                                                                          
012000     IF IN-BESTALLNING = 'PART '                                          
012010        MOVE SOP-SYMB-AREA TO SOP-SYMBOLIC-VARIABLES                      
012100        IF PARM-VCOM = 'US' OR 'CA' OR 'JP' OR 'AU' OR                    
012110                       'IN' OR 'KR' OR 'C1' OR 'TR' OR 'MY' OR            
012111                       'TH' OR 'TW' OR 'MX' OR 'ZA' OR 'BR'               
012200           MOVE 'W116S3'                  TO SOP-PROC-NAME                
012300        ELSE                                                              
013300           MOVE 'W116S2'   TO SOP-PROC-NAME                               
013800        END-IF                                                            
013900     END-IF                                                               
014000                                                                          
014100************* TOTALFIL ERSÄTTNINGAR                                       
014200                                                                          
014300     IF IN-BESTALLNING = 'SUPER'                                          
014310        IF PARM-VCOM = 'JP' OR 'AU' OR 'IN' OR 'KR' OR 'TR'               
014311                    OR 'C1' OR 'MY' OR 'TH' OR 'TW' OR 'MX'               
014312                    OR 'ZA' OR 'BR'                                       
014320           MOVE 'W116SF'             TO SOP-PROC-NAME                     
014323           MOVE SOP-SYMB-AREA   TO                                        
014324                           SOP-SYMBOLIC-VARIABLES                         
014330        ELSE                                                              
014400           IF PARM-VCOM = 'US'                                            
014500              MOVE 'W116S7'       TO SOP-PROC-NAME                        
014600           ELSE                                                           
014700              IF PARM-VCOM = 'CA'                                         
014800                 MOVE 'W116S8'    TO SOP-PROC-NAME                        
014900              ELSE                                                        
015600                 MOVE 'W116S1'    TO SOP-PROC-NAME                        
015700                 MOVE SOP-SYMB-AREA   TO                                  
015800                        SOP-SYMBOLIC-VARIABLES                            
015801              END-IF                                                      
015900           END-IF                                                         
016100        END-IF                                                            
016200     END-IF                                                               
016300                                                                          
016400                                                                          
016500************* TOTALFIL AKTIVA ARTIKLAR - PRICE                            
016600                                                                          
016700     IF IN-BESTALLNING = '     ' AND IN-SENDERTAG = 'W116870A'            
016710        IF PARM-VCOM = 'JP' OR 'AU' OR 'IN' OR 'KR' OR                    
016720                       'C1' OR 'US' OR 'CA' OR 'TR' OR 'MY' OR            
016721                       'TH' OR 'MX' OR 'ZA'                               
016722** ADD AT GO-LIVE                   OR 'BR'                               
016900           CONTINUE                                                       
017000        ELSE                                                              
018000           MOVE 'W116SB'      TO SOP-PROC-NAME                            
018110           MOVE SOP-SYMB-AREA TO SOP-SYMBOLIC-VARIABLES                   
018500        END-IF                                                            
018600     END-IF                                                               
018700                                                                          
018800************* TOTALFIL AKTIVA ARTIKLAR - VIPS                             
018900                                                                          
019000     IF IN-BESTALLNING = 'ACTIV'                                          
019010        IF PARM-VCOM = 'JP' OR 'AU' OR 'IN' OR 'KR' OR                    
019020                       'C1' OR 'US' OR 'CA' OR 'TR' OR 'MY' OR            
019021                       'TH' OR 'MX' OR 'ZA'                               
019022** ADD AT GO-LIVE                   OR 'BR'                               
020250           CONTINUE                                                       
020260        ELSE                                                              
020300           MOVE 'W116SC'      TO SOP-PROC-NAME                            
020410           MOVE SOP-SYMB-AREA TO SOP-SYMBOLIC-VARIABLES                   
020800        END-IF                                                            
020900     END-IF                                                               
021000                                                                          
021100     MOVE SPACE TO  SOP-DDPREFIX                                          
021200     MOVE 'O'   TO  SOP-SOPFUNC                                           
021300     MOVE ZERO  TO  SOP-ACTPASS-DATE                                      
021400                                                                          
021500     IF SOP-PROC-NAME = 'W116S1' OR 'W116S2' OR 'W116S3'                  
021700         OR 'W116S7' OR 'W116S8' OR 'W116S9' OR 'W116SB'                  
021800         OR 'W116SC' OR 'W116SF'                                          
021900                                                                          
022000        CALL W980SOP USING SOP-PARM-AREA                                  
022100                                                                          
022200        IF SOP-RETCODE > +8                                               
022300           CALL FELLOG                                                    
022400        END-IF                                                            
022500     END-IF                                                               
022600                                                                          
022700     DISPLAY 'RUTIN = ' SOP-PROC-NAME                                     
022800     DISPLAY 'SYMB  = ' SOP-SYMBOLIC-VARIABLES                            
022900     .                                                                    
023000     EJECT                                                                
023100 Z-FINIT SECTION.                                                         
023200                                                                          
023300     CLOSE SYSIN                                                          
023400           INFIL                                                          
023500     .                                                                    
023600     EJECT                                                                
023700 S01-LAES-SYSIN SECTION.                                                  
023800                                                                          
023900     READ SYSIN INTO PARM-AREA                                            
024000     AT END                                                               
024100        SET END-OF-SYSIN TO TRUE                                          
024200     END-READ                                                             
024300     .                                                                    
024400     EJECT                                                                
024500 S02-LAES-INFIL SECTION.                                                  
024600                                                                          
024700     READ INFIL INTO IN-AREA                                              
024800     AT END                                                               
024900        SET END-OF-INFIL TO TRUE                                          
025000                                                                          
025100     END-READ                                                             
025200     .                                                                    
