000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W5172400.                                                 
000400 AUTHOR.        K J HANSSON.                                              
000500 DATE-WRITTEN.  AUG 1981.                                                 
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION: SORTERAR OCH SUMMERAR RW1- OCH RW2- POSTER                 
001000*        PÅ IDPTYP, KDWRTYP, IDDC, IDDISTR, KDPRODSL O DAVVREG            
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500     SKIP2                                                                
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*--- INFIL:                                                               
001900     SELECT W51701                       ASSIGN TO UT-S-W51724D1.         
002000                                                                          
002100*--- UTFIL:                                                               
002200     SELECT W51713                       ASSIGN TO UT-S-W51724D2.         
002300                                                                          
002400*--- UTFIL:                                                               
002500     SELECT W51713X                      ASSIGN TO UT-S-W51724D3.         
002600                                                                          
002700*--- SORTFIL:                                                             
002800     SELECT SORTFIL                      ASSIGN TO UT-S-W51724DS.         
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W51701                                                               
003500     LABEL RECORD   STANDARD                                              
003600     RECORDING      V                                                     
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900*    -COPY  W517RW1  -L.                                                  
004000                                                                          
004100*    -COPY  W517RW2  -L.                                                  
004200     EJECT                                                                
004300 FD  W51713                                                               
004400     LABEL RECORD   STANDARD                                              
004500     RECORDING      V                                                     
004600     BLOCK CONTAINS 0.                                                    
004700                                                                          
004800*01  UTPOST    -COPY  W517RW1 -PRE RW1-  -L.                              
004900*01  UTPOST    -COPY  W517RW2 -PRE RW2-  -L.                              
005000     SKIP3                                                                
005010 FD  W51713X                                                              
005020     LABEL RECORD   STANDARD                                              
005030     RECORDING      F                                                     
005040     BLOCK CONTAINS 0.                                                    
005050                                                                          
005060*01  UTPOST    -COPY  W51724X -PRE RW1X- -L.                              
005080     SKIP3                                                                
005100 SD  SORTFIL.                                                             
005200                                                                          
005300*01  POST    -COPY  W517RW1 -PRE SORT-.                                   
005400                                                                          
005500*01  -COPY  W517RW2 -L.                                                   
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900                                                                          
006000*    -- CHECKED BY WY2000                                                 
006100 77  IDPGM                       PIC X(8) VALUE 'W5172400'.               
006200                                                                          
006300 01  GENERELLA-KONSTANTER.                                                
006400                                                                          
006500     03  JA                      PIC X(1)    VALUE 'J'.                   
006600     03  NEJ                     PIC X(1)    VALUE 'N'.                   
006700                                                                          
006800 01  POST-FINNS                  PIC X       VALUE 'J'.                   
006900                                                                          
007000 01  RETURKODER.                                                          
007100*                                                                         
007200     03  RKOD                    PIC S9(4)   COMP SYNC VALUE ZERO.        
007300     03  RKOD-16                 PIC S9(4)   COMP SYNC VALUE +16.         
007400                                                                          
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007900     EJECT                                                                
008000*01  -COPY  W0005          -PRE  POSTSUM-.                                
008100     EJECT                                                                
008200*                                                                         
008300 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
008400                                             'SORTWS-AREA-START '.        
008500 01  SORTWS-AREA.                                                         
008600     03  SWS-IDPTYP              PIC X(3)    VALUE SPACE.                 
008700     03  FILLER                  PIC X(97)   VALUE SPACE.                 
008800                                                                          
008900*01  FILLER  -PRE SORTW1-  -COPY W517RW1   -RED SORTWS-AREA.              
009000     EJECT                                                                
009100*01  FILLER  -PRE SORTW2-  -COPY W517RW2   -RED SORTWS-AREA.              
009200     EJECT                                                                
009300 01  U13-AREA.                                                            
009400*                                                                         
009500     03  U13-IDPTYP              PIC X(3)    VALUE SPACE.                 
009600     03  FILLER                  PIC X(97)   VALUE SPACE.                 
009700     SKIP2                                                                
009800*01  POST  -PRE U13RW1-   -COPY W517RW1   -RED U13-AREA.                  
009900     EJECT                                                                
010000*01  POST  -PRE U13RW2-   -COPY W517RW2   -RED U13RW1-POST.               
010100     EJECT                                                                
010101 01  FILLER                      PIC X(16) VALUE 'U13X-AREA'.             
010110*01  POST  -PRE U13XRW1-  -COPY W51724X.                                  
010120     EJECT                                                                
010200 PROCEDURE DIVISION.                                                      
010300                                                                          
010400                                                                          
010500     OPEN OUTPUT W51713                                                   
010600                 W51713X                                                  
010700                                                                          
010800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010900                                                                          
011000     SORT SORTFIL                                                         
011100          ASCENDING KEY SORT-IDPTYP                                       
011200                        SORT-KDWRTYP                                      
011300                        SORT-IDDC                                         
011400                        SORT-IDDISTR                                      
011500                        SORT-KDPRODSL                                     
011600                        SORT-DAVVREG                                      
011700          USING W51701                                                    
011800          OUTPUT PROCEDURE B-SORT-OUTPUT                                  
011900                                                                          
012000     IF SORT-RETURN > ZERO                                                
012100       DISPLAY IDPGM  ' RETURKOD FRÅN SORT STÖRRE ÄN NOLL'                
012200       MOVE RKOD-16 TO RKOD                                               
012300       CALL ABEND USING RKOD                                              
012400     END-IF                                                               
012500                                                                          
012600     CLOSE W51713                                                         
012610           W51713X                                                        
012700                                                                          
012800     MOVE 'S' TO POSTSUM-OPKOD                                            
012900     CALL POSTSUM USING POSTSUM-PARM                                      
013000                                                                          
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 B-SORT-OUTPUT SECTION.                                                   
013600                                                                          
013700     PERFORM S01-SORT-RETURN                                              
013800     PERFORM UNTIL POST-FINNS = NEJ                                       
013900       MOVE SORTWS-AREA TO U13-AREA                                       
014000       PERFORM S01-SORT-RETURN                                            
014100                                                                          
014200       EVALUATE U13-IDPTYP                                                
014300       WHEN 'RW1'                                                         
014400         PERFORM UNTIL POST-FINNS = NEJ            OR                     
014500            SORTW1-IDPTYP    NOT = U13RW1-IDPTYP   OR                     
014600            SORTW1-KDWRTYP   NOT = U13RW1-KDWRTYP  OR                     
014700            SORTW1-IDDC      NOT = U13RW1-IDDC     OR                     
014800            SORTW1-IDDISTR   NOT = U13RW1-IDDISTR  OR                     
014900            SORTW1-KDPRODSL  NOT = U13RW1-KDPRODSL OR                     
015000            SORTW1-DAVVREG   NOT = U13RW1-DAVVREG                         
015100           ADD SORTW1-SUARTSTD TO U13RW1-SUARTSTD                         
015200           ADD SORTW1-SUARTSJK TO U13RW1-SUARTSJK                         
015300           ADD SORTW1-SUARTFSG TO U13RW1-SUARTFSG                         
015400           PERFORM S01-SORT-RETURN                                        
015500         END-PERFORM                                                      
015600         PERFORM S02-SKRIV-W51713-RW1POST                                 
015610         PERFORM S03-SKRIV-W51713X-RW1POST                                
015700                                                                          
015800       WHEN 'RW2'                                                         
015900          PERFORM UNTIL POST-FINNS = NEJ           OR                     
016000            SORTW1-IDPTYP    NOT = U13RW1-IDPTYP   OR                     
016100            SORTW1-KDWRTYP   NOT = U13RW1-KDWRTYP  OR                     
016200            SORTW1-IDDC      NOT = U13RW1-IDDC     OR                     
016300            SORTW1-IDDISTR   NOT = U13RW1-IDDISTR  OR                     
016400            SORTW1-KDPRODSL  NOT = U13RW1-KDPRODSL OR                     
016500            SORTW1-DAVVREG   NOT = U13RW1-DAVVREG                         
016600             ADD SORTW2-SUARTSTD TO U13RW2-SUARTSTD                       
016700             PERFORM S01-SORT-RETURN                                      
016800          END-PERFORM                                                     
016900          PERFORM S04-SKRIV-W51713-RW2POST                                
017000       END-EVALUATE                                                       
017100     END-PERFORM                                                          
017200     .                                                                    
017300     EJECT                                                                
017400 S01-SORT-RETURN SECTION.                                                 
017500                                                                          
017600     RETURN SORTFIL INTO SORTWS-AREA                                      
017700     AT END                                                               
017800       MOVE NEJ TO POST-FINNS                                             
017900     NOT AT END                                                           
018000       MOVE 'W51701' TO POSTSUM-FDNAMN                                    
018100       MOVE 'W51724D1' TO POSTSUM-DDNAMN2                                 
018200       MOVE SWS-IDPTYP TO POSTSUM-TRANSTYP                                
018300       CALL POSTSUM USING POSTSUM-PARM                                    
018400     END-RETURN                                                           
018500     .                                                                    
018600     EJECT                                                                
018700 S02-SKRIV-W51713-RW1POST SECTION.                                        
018800                                                                          
018900     WRITE RW1-UTPOST FROM U13RW1-POST                                    
019000                                                                          
019100     MOVE 'W51713'   TO POSTSUM-FDNAMN                                    
019200     MOVE 'W51724D2' TO POSTSUM-DDNAMN2                                   
019300     MOVE U13-IDPTYP TO POSTSUM-TRANSTYP                                  
019400     CALL POSTSUM USING POSTSUM-PARM                                      
019500     .                                                                    
019600     SKIP3                                                                
019610 S03-SKRIV-W51713X-RW1POST SECTION.                                       
019620                                                                          
019621     MOVE U13RW1-IDDC      TO U13XRW1-IDDC                                
019622     MOVE U13RW1-IDDISTR   TO U13XRW1-IDDISTR                             
019623     MOVE U13RW1-KDPRODSL  TO U13XRW1-KDPRODSL                            
019624     MOVE U13RW1-DAVVREG   TO U13XRW1-DAVVREG                             
019625     MOVE U13RW1-SUARTSTD  TO U13XRW1-SUARTSTD                            
019626     MOVE U13RW1-SUARTSJK  TO U13XRW1-SUARTSJK                            
019627     MOVE U13RW1-SUARTFSG  TO U13XRW1-SUARTFSG                            
019628                                                                          
019630     WRITE RW1X-UTPOST   FROM U13XRW1-POST                                
019640                                                                          
019650     MOVE 'W51713X'        TO POSTSUM-FDNAMN                              
019660     MOVE 'W51724D3'       TO POSTSUM-DDNAMN2                             
019670     MOVE U13RW1-IDPTYP    TO POSTSUM-TRANSTYP                            
019680     CALL POSTSUM       USING POSTSUM-PARM                                
019690     .                                                                    
019691     SKIP3                                                                
019700 S04-SKRIV-W51713-RW2POST SECTION.                                        
019800                                                                          
019900     WRITE RW2-UTPOST FROM U13RW2-POST                                    
020000                                                                          
020100     MOVE 'W51713'   TO POSTSUM-FDNAMN                                    
020200     MOVE 'W51724D2' TO POSTSUM-DDNAMN2                                   
020300     MOVE U13-IDPTYP TO POSTSUM-TRANSTYP                                  
020400     CALL POSTSUM USING POSTSUM-PARM                                      
020500     .                                                                    
